{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RON.Store.Sqlite (
    Handle,
    Store,
    StoreT,
    fetchUpdates,
    loadOpLog,
    newHandle,
    runStore,
) where

import RON.Prelude

import Control.Concurrent.STM (TChan, dupTChan, writeTChan)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Data.List.NonEmpty (groupWith)
import Database.Selda (
    Attr ((:-)),
    MonadMask,
    SeldaT,
    SqlRow,
    SqlType,
    Table,
    ascending,
    distinct,
    fromSql,
    insert_,
    literal,
    mkLit,
    order,
    primary,
    query,
    restrict,
    select,
    table,
    transaction,
    tryCreateTable,
    (!),
    (.==),
 )
import Database.Selda qualified
import Database.Selda.Backend (
    Lit (LCustom),
    SeldaConnection,
    SqlTypeRep (TBlob),
    runSeldaT,
 )
import Database.Selda.SQLite (SQLite, sqliteOpen)
import System.Random.TF (newTFGen)
import System.Random.TF.Instances (random)
import UnliftIO (
    MonadUnliftIO,
    atomically,
    catch,
    newBroadcastTChanIO,
    newIORef,
    throwIO,
 )
import UnliftIO.Directory (makeAbsolute)

import RON.Data.VersionVector (VV, (·≻))
import RON.Epoch (EpochClockT, getCurrentEpochTime, runEpochClock)
import RON.Error (Error, errorContext)
import RON.Event (
    OriginVariety (ApplicationSpecific),
    Replica,
    ReplicaClock,
    mkReplica,
 )
import RON.Store.Class (MonadStore)
import RON.Store.Class qualified
import RON.Text.Parse (parsePayload, parseUuid)
import RON.Text.Serialize (serializePayload, serializeUuid)
import RON.Types (Payload, UUID)
import RON.Types qualified as RON
import RON.Types.Experimental (Patch (..))
import RON.Util.Word (Word60, ls60)

instance SqlType UUID where
    sqlType _ = TBlob
    defaultValue = undefined
    fromSql = either error id . parseUuid . fromSql @ByteStringL
    mkLit = LCustom TBlob . mkLit @ByteStringL . serializeUuid

instance SqlType Payload where
    sqlType _ = TBlob
    defaultValue = undefined
    fromSql = either error id . parsePayload . fromSql @ByteStringL
    mkLit = LCustom TBlob . mkLit @ByteStringL . serializePayload

data Op = Op
    { object :: UUID
    , event :: UUID
    , ref :: UUID
    , payload :: Payload
    }
    deriving stock (Generic)
    deriving anyclass (SqlRow)

opTable :: Table Op
opTable = table "Op" [#event :- primary]

opToDatabase :: UUID -> RON.Op -> Op
opToDatabase object RON.Op{opId, refId, payload} =
    Op{event = opId, ref = refId, object, payload = payload}

opFromDatabase :: Op -> RON.Op
opFromDatabase Op{event, ref, payload} =
    RON.Op{opId = event, refId = ref, payload = payload}

data Handle = Handle
    { clock :: IORef Word60
    , dbConn :: SeldaConnection SQLite
    , onNewPatch :: TChan Patch
    {- ^ A channel of changes in the database.
    This is a broadcast channel, so you MUST NOT read from it directly,
    call 'fetchUpdates' to read from derived channel instead.
    -}
    , replica :: Replica
    }

newtype StoreT m a = Store (ReaderT Handle (EpochClockT m) a)
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadUnliftIO
        , ReplicaClock
        )

type Store = StoreT (LoggingT IO)

instance (MonadUnliftIO m) => MonadError Error (StoreT m) where
    throwError = throwIO
    catchError = catch

instance (MonadLogger m, MonadUnliftIO m) => MonadStore (StoreT m) where
    listObjects = listObjects
    appendPatch = appendPatch
    loadWholeObjectLog = loadWholeObjectLog

instance MonadTrans StoreT where
    lift = Store . lift @(ReaderT _) . lift @EpochClockT

listObjects :: (MonadUnliftIO m) => StoreT m [UUID]
listObjects = errorContext "listObjects @Store" $ runDB selectDistinctObject

appendPatch :: (MonadLogger m, MonadUnliftIO m) => Patch -> StoreT m ()
appendPatch Patch{object, log} =
    errorContext "appendPatch @Store" do
        opsInserted <-
            runDB do
                catMaybes <$> for (toList log) \op -> do
                    transaction do
                        existing <- query do
                            row <- select opTable
                            restrict $ row ! #event .== literal op.opId
                            pure $ row ! #event
                        if null existing then do
                            insert_ opTable [opToDatabase object op]
                            pure $ Just op
                        else
                            pure Nothing
        case opsInserted of
            [] -> pure ()
            op : ops -> do
                Handle{onNewPatch} <- Store ask
                atomically do
                    writeTChan onNewPatch Patch{object, log = op :| ops}

loadWholeObjectLog ::
    (MonadUnliftIO m) => UUID -> VV -> StoreT m [RON.Op]
loadWholeObjectLog object version =
    errorContext "loadWholeObjectLog @Store"
        $ runDB do
            ops <-
                query do
                    op <- select opTable
                    restrict $ op ! #object .== literal object
                    order (op ! #event) ascending
                    pure op
            pure [opFromDatabase op | op <- ops, op.event ·≻ version]

loadOpLog :: (MonadUnliftIO m) => StoreT m [Patch]
loadOpLog =
    errorContext "loadOpLog"
        $ runDB do
            oplog <-
                query do
                    op <- select opTable
                    order (op ! #event) ascending
                    pure op
            pure
                [ Patch object $ opFromDatabase <$> ops
                | ops@(Op{object} :| _) <- groupWith (.object) oplog
                ]

runDB :: (MonadIO m) => SeldaT SQLite IO a -> StoreT m a
runDB action = do
    Handle{dbConn} <- Store ask
    liftIO $ (`runSeldaT` dbConn) action

runStore :: Handle -> StoreT m a -> m a
runStore h@Handle{replica, clock} (Store action) =
    runEpochClock replica clock $ runReaderT action h

fetchUpdates :: (MonadIO m) => Handle -> m (TChan Patch)
fetchUpdates Handle{onNewPatch} = atomically $ dupTChan onNewPatch

selectDistinctObject :: (MonadIO m, MonadMask m) => SeldaT b m [UUID]
selectDistinctObject =
    query
        $ distinct do
            ops <- select opTable
            pure $ ops ! #object

{- | Create new Store handle.
If no replica id found in the DB, generates a random one.
-}
newHandle ::
    (MonadLogger m, MonadMask m, MonadUnliftIO m) => FilePath -> m Handle
newHandle dbfile' = do
    time <- getCurrentEpochTime -- TODO advance to the last timestamp
    -- in the database
    clock <- newIORef time
    dbfile <- makeAbsolute dbfile'
    dbConn <- sqliteOpen dbfile
    (`runSeldaT` dbConn) $ tryCreateTable opTable
    onNewPatch <- newBroadcastTChanIO
    replica <- newReplica -- TODO load replica id from the database
    pure Handle{clock, dbConn, onNewPatch, replica}

newReplica :: (MonadIO m) => m Replica
newReplica = do
    replicaId <- fst . random <$> liftIO newTFGen
    pure $ mkReplica ApplicationSpecific $ ls60 replicaId
