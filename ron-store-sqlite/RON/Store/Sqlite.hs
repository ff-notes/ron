{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Store.Sqlite (
  Handle, Store, StoreT, fetchUpdates, loadOpLog, newHandle, runStore,
) where

import           RON.Prelude

import           Control.Concurrent.STM (TChan, dupTChan, writeTChan)
import           Control.Monad.Logger (LoggingT, MonadLogger, monadLoggerLog,
                                       runLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty (groupWith)
import           Data.Pool (Pool)
import qualified Data.Text as Text
import           Database.Persist (Entity (..),
                                   PersistValue (PersistByteString),
                                   SelectOpt (Asc), insertUnique, selectList,
                                   (==.))
import           Database.Persist.Sql (PersistField, PersistFieldSql,
                                       SqlBackend, rawSql, runMigration,
                                       runSqlPool, sqlType, unSingle)
import qualified Database.Persist.Sql
import           Database.Persist.Sqlite (createSqlitePool)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)
import           UnliftIO (MonadUnliftIO, atomically, catch,
                           newBroadcastTChanIO, newIORef, throwIO, withRunInIO)
import           UnliftIO.Directory (makeAbsolute)

import           RON.Data.VersionVector (VV, (·≻))
import           RON.Epoch (EpochClockT, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error, errorContext)
import           RON.Event (OriginVariety (ApplicationSpecific), Replica,
                            ReplicaClock, mkReplica)
import           RON.Store.Class (MonadStore)
import qualified RON.Store.Class
import           RON.Text.Parse (parsePayload, parseUuid)
import           RON.Text.Serialize (serializePayload, serializeUuid)
import           RON.Types (Payload, UUID)
import qualified RON.Types as RON
import           RON.Types.Experimental (Patch (..))
import           RON.Util.Word (Word60, ls60)

instance PersistField UUID where
  toPersistValue = PersistByteString . BSL.toStrict . serializeUuid
  fromPersistValue = \case
    PersistByteString bs -> fmapL Text.pack $ parseUuid $ BSL.fromStrict bs
    _                    -> Left "expected PersistByteString"

instance PersistFieldSql UUID where
  sqlType _ = sqlType (Proxy @ByteString)

instance PersistField Payload where
  toPersistValue = PersistByteString . BSL.toStrict . serializePayload
  fromPersistValue = \case
    PersistByteString bs -> fmapL Text.pack $ parsePayload $ BSL.fromStrict bs
    _                    -> Left "expected PersistByteString"

instance PersistFieldSql Payload where
  sqlType _ = sqlType (Proxy @ByteString)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Op
      event   UUID -- op own id
      ref     UUID -- parent op id
      object  UUID -- enclosing object (itself for root op)
      payload Payload

      UniqueEvent event
  |]

opToDatabase :: UUID -> RON.Op -> Op
opToDatabase opObject RON.Op{opId, refId, payload} =
  Op{opEvent = opId, opRef = refId, opObject, opPayload = payload}

opFromDatabase :: Op -> RON.Op
opFromDatabase Op{opEvent, opRef, opPayload} =
  RON.Op{opId = opEvent, refId = opRef, payload = opPayload}

data Handle = Handle
  { clock      :: IORef Word60
  , dbPool     :: Pool SqlBackend
  , onNewPatch :: TChan Patch
    -- ^ A channel of changes in the database.
    -- This is a broadcast channel, so you MUST NOT read from it directly,
    -- call 'fetchUpdates' to read from derived channel instead.
  , replica :: Replica
  }

newtype StoreT m a = Store (ReaderT Handle (EpochClockT m) a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO, ReplicaClock)

type Store = StoreT (LoggingT IO)

instance MonadUnliftIO m => MonadError Error (StoreT m) where
  throwError = throwIO
  catchError = catch

instance (MonadLogger m, MonadUnliftIO m) => MonadStore (StoreT m) where
  listObjects       = listObjects'
  appendPatch       = appendPatch'
  loadObjectLog     = loadObjectLog'
  getObjectVersion  = undefined

instance MonadTrans StoreT where
  lift = Store . lift @(ReaderT _) . lift @EpochClockT

listObjects' :: (MonadLogger m, MonadUnliftIO m) => StoreT m [UUID]
listObjects' = errorContext "listObjects @Store" $ runDB selectDistinctObject

appendPatch' :: (MonadLogger m, MonadUnliftIO m) => Patch -> StoreT m ()
appendPatch' Patch{object, log} =
  errorContext "appendPatch @Store" do
    opsInserted <-
      runDB $
      catMaybes . toList <$>
      for log \op ->
        (op <$) <$> insertUnique (opToDatabase object op)
        -- if successful, return op
    case opsInserted of
      [] -> pure ()
      op : ops -> do
        Handle{onNewPatch} <- Store ask
        atomically $ writeTChan onNewPatch Patch{object, log = op :| ops}

loadObjectLog' ::
  (MonadLogger m, MonadUnliftIO m) => UUID -> VV -> StoreT m [[RON.Op]]
loadObjectLog' object version =
  errorContext "loadObjectLog @Store" do
    ops <- runDB $ selectList [OpObject ==. object] [Asc OpEvent]
    pure
      [[opFromDatabase op | Entity _ op@Op{opEvent} <- ops, opEvent ·≻ version]]

loadOpLog :: (MonadLogger m, MonadUnliftIO m) => StoreT m [Patch]
loadOpLog =
  errorContext "loadOpLog" do
    oplog <- runDB $ map entityVal <$> selectList [] [Asc OpEvent]
    pure
      [ Patch opObject $ opFromDatabase <$> ops
      | ops@(Op{opObject} :| _) <- groupWith opObject oplog
      ]

runDB ::
  (MonadUnliftIO m, MonadLogger m) =>
  ReaderT SqlBackend (LoggingT (ResourceT IO)) a -> StoreT m a
runDB action = do
  Handle{dbPool} <- Store ask
  let action' =
        (`runSqlPool` dbPool) do
          runMigration migrateAll
          action
  lift $
    withRunInIO \unlift -> let
      monadLoggerLog' loc src lvl msg =
        unlift $ monadLoggerLog loc src lvl msg
      in runResourceT $ runLoggingT action' monadLoggerLog'

runStore :: Handle -> StoreT m a -> m a
runStore h@Handle{replica, clock} (Store action) =
  runEpochClock replica clock $ runReaderT action h

fetchUpdates :: MonadIO m => Handle -> m (TChan Patch)
fetchUpdates Handle{onNewPatch} = atomically $ dupTChan onNewPatch

selectDistinctObject :: MonadIO m => ReaderT SqlBackend m [UUID]
selectDistinctObject =
  map unSingle <$> rawSql "SELECT DISTINCT object FROM Op" []

-- | Create new Store handle.
-- If no replica id found in the DB, generates a random one.
newHandle :: (MonadLogger m, MonadUnliftIO m) => FilePath -> m Handle
newHandle dbfile' = do
  time        <- getCurrentEpochTime  -- TODO advance to the last timestamp
                                      -- in the database
  clock       <- newIORef time
  dbfile      <- makeAbsolute dbfile'
  dbPool      <- runLoggerIO $ createSqlitePool (Text.pack dbfile) 1
  onNewPatch  <- newBroadcastTChanIO
  replica     <- newReplica -- TODO load replica id from the database
  pure Handle{clock, dbPool, onNewPatch, replica}

newReplica :: MonadIO m => m Replica
newReplica = do
  replicaId <- fst . random <$> liftIO newTFGen
  pure $ mkReplica ApplicationSpecific $ ls60 replicaId

runLoggerIO :: (MonadLogger m, MonadUnliftIO m) => LoggingT IO a -> m a
runLoggerIO action =
  withRunInIO \run ->
    runLoggingT action \loc src lvl msg -> run $ monadLoggerLog loc src lvl msg
