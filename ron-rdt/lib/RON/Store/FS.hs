{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RON.Store.FS (Handle, Store, debugDump, newHandle, runStore) where

import           RON.Prelude

import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable (find)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, makeAbsolute)
import           System.FilePath ((</>))
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Data.VersionVector (VersionVector, (·≼))
import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error (..), MonadE, liftEitherString, tryIO)
import           RON.Event (EpochTime, ReplicaClock, ReplicaId,
                            applicationSpecific, getEventUuid)
import           RON.Store (MonadStore (..))
import           RON.Text.Parse (parseOpenFrame)
import           RON.Text.Serialize.Experimental (serializeOpenFrame)
import           RON.Types (Op (..), UUID)
import           RON.Util.Word (Word60, leastSignificant60)
import qualified RON.UUID as UUID

-- | Store handle (uses the “Handle pattern”).
data Handle = Handle
  { clock   :: IORef EpochTime
  , dataDir :: FilePath
  -- fsWatchManager    :: FSNotify.WatchManager,
  -- stopWatching      :: IORef (Maybe StopListening),
  -- onDocumentChanged :: TChan RawDocId,
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
  listObjects = do
    Handle{dataDir} <- Store ask
    objectDirs <-
      tryIO $ do
        exists <- doesDirectoryExist dataDir
        if exists then listDirectory dataDir else pure []
    traverse uuidFromFileName objectDirs

  appendPatch = appendPatchFS

  loadObjectLog = loadObjectLogFS

loadObjectLogFS :: UUID -> VersionVector -> Store [[Op]]
loadObjectLogFS objectId version = do
  Handle{dataDir} <- Store ask
  let objectLogsDir = dataDir </> uuidToFileName objectId </> "log"
  objectExists <- tryIO $ doesDirectoryExist objectLogsDir
  if objectExists then do
    patchNames <- tryIO $ listDirectory objectLogsDir
    fmap catMaybes . for patchNames $ \patchName -> do
      patchTimestamp <- uuidFromFileName patchName
      if patchTimestamp ·≼ version then
        pure Nothing
      else do
        let patchFile = objectLogsDir </> patchName
        patchContent <- tryIO $ BSL.readFile patchFile
        patch <- liftEitherString $ parseOpenFrame patchContent
        pure $ Just patch
  else
    pure []

appendPatchFS :: UUID -> [Op] -> Store ()
appendPatchFS objectId patch = do
  Handle{dataDir} <- Store ask
  let objectLogsDir = dataDir </> uuidToFileName objectId </> "log"
  tryIO $ createDirectoryIfMissing True objectLogsDir
  patchVersion <- getEventUuid
  let patchFile = objectLogsDir </> uuidToFileName patchVersion
  tryIO $ BSL.writeFile patchFile $ serializeOpenFrame patch

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
  newHandleWithReplicaId hDataDir $ leastSignificant60 replicaId

newHandleWithReplicaId :: FilePath -> Word60 -> IO Handle
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

uuidFromFileName :: MonadE m => FilePath -> m UUID
uuidFromFileName =
  maybe (throwError "UUID.decodeBase32: filename is not a valid UUID") pure
  . UUID.decodeBase32

uuidToFileName :: UUID -> FilePath
uuidToFileName = UUID.encodeBase32

debugDump :: FilePath -> IO ()
debugDump dataDir = do
  objectDirs <- do
    exists <- doesDirectoryExist dataDir
    if exists then listDirectory dataDir else pure []
  for_ (sort objectDirs) $ \objectDir -> do
    let logsDir = dataDir </> objectDir </> "log"
    logs <- listDirectory logsDir
    for_ (sort logs) $ \logName -> do
      let logPath = logsDir </> logName
      BSL.putStr =<< BSL.readFile logPath
    BSLC.putStrLn ""
