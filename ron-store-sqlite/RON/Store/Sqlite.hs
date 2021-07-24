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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Store.Sqlite (Handle, fetchUpdates, newHandle, runStore) where

import           RON.Prelude

import           Control.Concurrent.STM (TChan, atomically, dupTChan,
                                         newBroadcastTChanIO)
import           Control.Monad.Logger (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Lazy as BSL
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
import           System.Directory (makeAbsolute)
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Data.VersionVector (VV, (·≻))
import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error, MonadE, errorContext, tryIO)
import           RON.Event (OriginVariety (ApplicationSpecific), Replica,
                            ReplicaClock, mkReplica)
import           RON.Store.Class (MonadStore)
import qualified RON.Store.Class
import           RON.Text.Parse (parsePayload, parseUuid)
import           RON.Text.Serialize (serializePayload, serializeUuid)
import           RON.Types (Payload, UUID)
import qualified RON.Types as RON
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

data Handle = Handle
  { clock  :: IORef Word60
  , dbPool :: Pool SqlBackend
  , onOp   :: TChan RON.Op
    -- ^ A channel of changes in the database.
    -- This is a broadcast channel, so you MUST NOT read from it directly,
    -- call 'fetchUpdates' to read from derived channel instead.
  , replica :: Replica
  }

newtype Store a = Store (ExceptT Error (ReaderT Handle EpochClock) a)
  deriving (Applicative, Functor, Monad, MonadE, MonadIO, ReplicaClock)

instance MonadStore Store where

  listObjects = errorContext "listObjects @Store" $ runDB selectDistinctObject

  appendPatchFromOneOrigin opObject ops =
    errorContext "appendPatchFromOneOrigin @Store" $
    runDB $
    for_ ops \RON.Op{opId, refId, payload} ->
      insertUnique
        Op{opEvent = opId, opRef = refId, opObject, opPayload = payload}

  loadObjectLog = loadObjectLog'

  getObjectVersion = undefined

loadObjectLog' :: UUID -> VV -> Store [[RON.Op]]
loadObjectLog' object version =
  errorContext "loadObjectLog @Store" do
    ops <- runDB $ selectList [OpObject ==. object] [Asc OpEvent]
    pure
      [ [ RON.Op{opId = opEvent, refId = opRef, payload = opPayload}
        | Entity _ Op{opEvent, opRef, opPayload} <- ops
        , opEvent ·≻ version
        ]
      ]

runDB :: ReaderT SqlBackend (LoggingT (ResourceT IO)) a -> Store a
runDB action =
  Store do
    Handle{dbPool} <- ask
    tryIO $
      runResourceT $
      runStderrLoggingT $
      runSqlPool
        (do
          runMigration migrateAll
          action)
        dbPool

runStore :: Handle -> Store a -> IO a
runStore h@Handle{replica, clock} (Store action) = do
  res <- runEpochClock replica clock $ (`runReaderT` h) $ runExceptT action
  either throwIO pure res

fetchUpdates :: Handle -> IO (TChan RON.Op)
fetchUpdates Handle{onOp} = atomically $ dupTChan onOp

selectDistinctObject :: MonadIO m => ReaderT SqlBackend m [UUID]
selectDistinctObject =
  map unSingle <$> rawSql "SELECT DISTINCT object FROM Op" []

-- | Create new Store handle.
-- If no replica id found in the DB, uses MAC address for replica id
-- or generates a random one.
newHandle :: FilePath -> IO (Maybe Handle)
newHandle dbfile' = do
  time    <- getCurrentEpochTime  -- TODO advance to the last timestamp
                                  -- in database
  clock   <- newIORef time
  dbfile  <- makeAbsolute dbfile'
  dbPool  <- runStderrLoggingT $ createSqlitePool (Text.pack dbfile) 1
  onOp    <- newBroadcastTChanIO
  replica <- newReplica -- TODO load replica id from database
  pure $ Just Handle{..}

newReplica :: IO Replica
newReplica = do
  replicaId <- fst . random <$> newTFGen
  pure $ mkReplica ApplicationSpecific $ ls60 replicaId
