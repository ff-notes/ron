{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module RON.Store.FS (
  Handle,
  Store,
  debugDump,
  fetchUpdates,
  newHandle,
  newHandleWithReplica,
  runStore,
  -- * Object subscriptions
  subcribeToObject,
  readObjectSubscriptions,
) where

import           RON.Prelude

import           Control.Concurrent (MVar, newMVar, withMVar)
import           Control.Concurrent.STM (TChan, atomically, dupTChan,
                                         newBroadcastTChanIO, writeTChan)
import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable (find)
import qualified Data.Set as Set
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, makeAbsolute)
import           System.FileLock (SharedExclusive (Exclusive), tryLockFile)
import           System.FilePath ((</>))
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Data.VersionVector (VV, mkVV, (·≼))
import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error (..), MonadE, errorContext, liftEitherString,
                            throwErrorText, tryIO)
import           RON.Event (OriginVariety (ApplicationSpecific), Replica,
                            ReplicaClock, getEventUuid, mkReplica)
import           RON.Store (MonadStore (..))
import           RON.Text.Parse (parseOpenFrame)
import           RON.Text.Serialize.Experimental (serializeOpenFrame)
import           RON.Types (Op (..), UUID)
import qualified RON.UUID as UUID
import           RON.Util.Word (Word60, leastSignificant60)

-- | Store handle (uses the “Handle pattern”).
data Handle = Handle
  { clock           :: IORef Word60
  , dataDir         :: FilePath
  , onObjectChanged :: TChan UUID
    -- ^ A channel of changes in the database.
    -- This is a broadcast channel, so you MUST NOT read from it directly,
    -- call 'fetchUpdates' to read from derived channel instead.
  , opLock              :: MVar ()
  , replica             :: Replica
  , objectSubscriptions :: IORef (Set UUID)
  }

newtype Store a = Store (ExceptT Error (ReaderT Handle EpochClock) a)
  deriving
    (Applicative, Functor, Monad, MonadError Error, MonadIO, ReplicaClock)

instance MonadStore Store where
  listObjects =
    errorContext "Store.listObjects" $ do
      Handle{dataDir} <- Store ask
      objectDirs <-
        tryIO $ do
          exists <- doesDirectoryExist dataDir
          if exists then listDirectoryDirs dataDir else pure []
      traverse uuidFromFileName objectDirs

  appendPatch = appendPatchFS

  loadObjectLog = loadObjectLogFS

  getObjectVersion objectId =
    errorContext "Store.getObjectVersion" $ do
      patchNames <- getObjectPatches objectId
      mkVV <$> for patchNames uuidFromFileName

askObjectLogsDir :: MonadReader Handle m => UUID -> m FilePath
askObjectLogsDir objectId =
  asks $ (</> uuidToFileName objectId </> "log") . dataDir

getObjectPatches :: UUID -> Store [FilePath]
getObjectPatches objectId = do
  objectLogsDir <- Store $ askObjectLogsDir objectId
  objectExists  <- tryIO $ doesDirectoryExist objectLogsDir
  if objectExists then tryIO $ listDirectory objectLogsDir else pure []

loadObjectLogFS :: UUID -> VV -> Store [[Op]]
loadObjectLogFS objectId version = do
  objectLogsDir <- Store $ askObjectLogsDir objectId
  patchNames    <- getObjectPatches objectId
  fmap catMaybes . for patchNames $ \patchName -> do
    patchTimestamp <- uuidFromFileName patchName
    if patchTimestamp ·≼ version then
      pure Nothing
    else do
      let patchFile = objectLogsDir </> patchName
      patchContent <- tryIO $ BSL.readFile patchFile
      patch <- liftEitherString $ parseOpenFrame patchContent
      pure $ Just patch

appendPatchFS :: UUID -> [Op] -> Store ()
appendPatchFS objectId patch = do
  Handle{dataDir, onObjectChanged} <- Store ask
  let objectLogsDir = dataDir </> uuidToFileName objectId </> "log"
  tryIO $ createDirectoryIfMissing True objectLogsDir
  patchVersion <- getEventUuid
  let patchFile = objectLogsDir </> uuidToFileName patchVersion
  tryIO $ BSL.writeFile patchFile $ serializeOpenFrame patch
  tryIO $ atomically $ writeTChan onObjectChanged objectId

-- | Run a 'Store' action
runStore :: Handle -> Store a -> IO a
runStore h@Handle{replica, clock, opLock} (Store action) = do
  res <-
    withMVar opLock $ \_ ->
      runEpochClock replica clock $ (`runReaderT` h) $ runExceptT action
  either throwIO pure res

-- | Create new storage handle.
-- Uses MAC address for replica id or generates a random one.
newHandle :: FilePath -> IO (Maybe Handle)
newHandle dataDir = do
  macAddress <- getMacAddress
  replicaId  <-
    case macAddress of
      Just macAddress' -> pure macAddress'
      Nothing          -> fst . random <$> newTFGen
  newHandleWithReplica dataDir $ leastSignificant60 replicaId

newHandleWithReplica :: FilePath -> Word60 -> IO (Maybe Handle)
newHandleWithReplica dataDir' replicaId = do
  dataDir <- makeAbsolute dataDir'
  let appLockFile = dataDir </> "applock"
  mLock <- tryLockFile appLockFile Exclusive
  case mLock of
    Nothing -> pure Nothing
    Just _ -> do
      time            <- getCurrentEpochTime
      clock           <- newIORef time
      onObjectChanged <- newBroadcastTChanIO
      opLock          <- newMVar ()
      let replica = mkReplica ApplicationSpecific replicaId
      objectSubscriptions <- newIORef mempty
      pure $ Just Handle{..}

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
uuidFromFileName name =
  maybe
    ( throwErrorText $
      "UUID.decodeBase32: file name " <> show name <> " is not a valid UUID"
    )
    pure
    (UUID.decodeBase32 name)

uuidToFileName :: UUID -> FilePath
uuidToFileName = UUID.encodeBase32

debugDump :: FilePath -> IO ()
debugDump dataDir = do
  objectDirs <- do
    exists <- doesDirectoryExist dataDir
    if exists then listDirectory dataDir else pure []
  for_ (sort objectDirs) \objectDir -> do
    isDir <- doesDirectoryExist $ dataDir </> objectDir
    when isDir $ do
      let logsDir = dataDir </> objectDir </> "log"
      logs <- listDirectory logsDir
      for_ (sort logs) $ \logName -> do
        let logPath = logsDir </> logName
        BSL.putStr =<< BSL.readFile logPath
      BSLC.putStrLn ""

fetchUpdates :: Handle -> IO (TChan UUID)
fetchUpdates Handle{onObjectChanged} = atomically $ dupTChan onObjectChanged

subcribeToObject :: Handle -> UUID -> IO ()
subcribeToObject Handle{objectSubscriptions} object =
  atomicModifyIORef' objectSubscriptions $ (,()) . Set.insert object

readObjectSubscriptions :: Handle -> IO (Set UUID)
readObjectSubscriptions Handle{objectSubscriptions} =
  readIORef objectSubscriptions

listDirectoryDirs :: FilePath -> IO [FilePath]
listDirectoryDirs dir =
  listDirectory dir >>= filterM \name -> doesDirectoryExist (dir </> name)
