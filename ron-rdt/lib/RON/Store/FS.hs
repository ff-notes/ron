{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Store.FS (newHandle, runStore) where

import           RON.Prelude

import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (find)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           RON.Types.Experimental (CollectionName)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, makeAbsolute)
import           System.FilePath ((</>))
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error, MonadE, tryIO)
import           RON.Event (EpochTime, ReplicaClock, ReplicaId,
                            applicationSpecific, getEventUuid)
import           RON.Store (MonadStore (..))
import           RON.Text.Serialize.Experimental (serializePatch)
import           RON.Types (Op, UUID)
import qualified RON.UUID as UUID

-- | Store handle (uses the “Handle pattern”).
data Handle = Handle
  { clock   :: IORef EpochTime
  , dataDir :: FilePath
  -- fsWatchManager    :: FSNotify.WatchManager,
  -- stopWatching      :: IORef (Maybe StopListening),
  -- onDocumentChanged :: TChan (CollectionName, RawDocId),
  -- ^ A channel of changes in the database.
  -- To activate it, call 'startWatching'.
  -- You should NOT read from it directly,
  -- call 'subscribe' to read from derived channel instead.
  , replica :: ReplicaId
  }

newtype Store a = Store (ExceptT Error (ReaderT Handle EpochClock) a)
  deriving
    (Applicative, Functor, Monad, MonadError Error, MonadIO, ReplicaClock)

instance MonadStore Store where
  getCollections = do
    Handle{dataDir} <- Store ask
    collectionDirs  <- liftIO $ do
      entries <- listDirectory dataDir
      filterM (doesDirectoryExist . (dataDir </>)) entries
    pure $ map collectionNameFromFileName collectionDirs

  getObjectsImpl collectionName = do
    Handle{dataDir} <- Store ask
    let collectionDir = dataDir </> collectionNameToFileName collectionName
    objectDirs <-
      liftIO $ do
        exists <- doesDirectoryExist collectionDir
        if exists then listDirectory collectionDir else pure []
    traverse uuidFromFileName objectDirs

  addPatch = addPatchFS

  -- loadCachedObjectImpl = loadCachedObjectImplFS

addPatchFS :: CollectionName -> UUID -> [Op] -> Store ()
addPatchFS collectionName objectId patch = do
  Handle{dataDir} <- Store ask
  let
    objectLogsDir
      =   dataDir
      </> collectionNameToFileName collectionName
      </> uuidToFileName objectId
      </> "log"
  liftIO $ createDirectoryIfMissing True objectLogsDir
  patchVersion <- getEventUuid
  let patchFile = objectLogsDir </> uuidToFileName patchVersion
  tryIO $ BSL.writeFile patchFile $ serializePatch patch

-- loadCachedObjectImplFS :: CollectionName -> UUID -> Store (Maybe WireStateChunk)
-- loadCachedObjectImplFS collectionName objectId = do
--   Handle{dataDir} <- Store ask
--   let
--     objectDir
--       =   dataDir
--       </> collectionNameToFileName collectionName
--       </> uuidToFileName objectId
--   objectPathExists <- liftIO $ doesPathExist objectDir
--   if objectPathExists then
--     Just <$> do
--       objectIsDir <- liftIO $ doesDirectoryExist objectDir
--       unless objectIsDir $
--         throwErrorString $
--         unwords ["object path", objectDir, "is expected to be a directory"]
--       let objectCachedStateDir = objectDir </> "state"
--       objectStateFiles <- liftIO $ listDirectory objectCachedStateDir
--       case objectStateFiles of
--         [objectStateFile] -> do
--           objectStateSerialized <- liftIO $ BSL.readFile objectStateFile
--           liftEitherString $ parseStateChunk objectStateSerialized
--         _ -> undefined
--   else
--     pure Nothing

-- | Run a 'Store' action
runStore :: Handle -> Store a -> IO a
runStore h@Handle{replica, clock} (Store action) = do
  res <- runEpochClock replica clock $ (`runReaderT` h) $ runExceptT action
  either throwIO pure res

-- | Create new storage handle.
-- Uses MAC address for replica id or generates a random one.
newHandle :: FilePath -> IO Handle
newHandle hDataDir = do
  macAddress <- getMacAddress
  replicaId  <-
    case macAddress of
      Just macAddress' -> pure macAddress'
      Nothing          -> fst . random <$> newTFGen
  newHandleWithReplicaId hDataDir replicaId

newHandleWithReplicaId :: FilePath -> Word64 -> IO Handle
newHandleWithReplicaId dataDir' replicaId = do
  dataDir <- makeAbsolute dataDir'
  time    <- getCurrentEpochTime
  clock   <- newIORef time
  -- fsWatchManager <- FSNotify.startManager
  -- stopWatching      <- newIORef Nothing
  -- onDocumentChanged <- newBroadcastTChanIO
  let replica = applicationSpecific replicaId
  pure Handle{..}

getMacAddress :: IO (Maybe Word64)
getMacAddress =
  do
    macAddress <- getMac
    pure $ decodeMac <$> macAddress
  where
    getMac = find (/= minBound) . map mac <$> getNetworkInterfaces
    decodeMac (MAC b5 b4 b3 b2 b1 b0)
      = (fromIntegral b5 `shiftL` 40)
      + (fromIntegral b4 `shiftL` 32)
      + (fromIntegral b3 `shiftL` 24)
      + (fromIntegral b2 `shiftL` 16)
      + (fromIntegral b1 `shiftL` 8)
      +  fromIntegral b0

collectionNameFromFileName :: FilePath -> CollectionName
collectionNameFromFileName = Text.encodeUtf8 . Text.pack

collectionNameToFileName :: CollectionName -> FilePath
collectionNameToFileName = Text.unpack . Text.decodeUtf8

uuidFromFileName :: MonadE m => FilePath -> m UUID
uuidFromFileName =
  maybe (throwError "UUID.decodeBase32: filename is not a valid UUID") pure
  . UUID.decodeBase32

uuidToFileName :: UUID -> FilePath
uuidToFileName = UUID.encodeBase32
