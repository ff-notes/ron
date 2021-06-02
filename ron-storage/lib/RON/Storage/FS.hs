{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A real-world file storage.
--
-- Typical usage:
--
-- @
-- import RON.Storage.FS as Storage
--
-- main = do
--     let dataDir = ".\/data\/"
--     h <- Storage.'newHandle' dataDir
--     'runStorage' h $ do
--         obj <- 'newObjectState' Note{active = True, text = "Write an example"}
--         'createDocument' obj
-- @
module RON.Storage.FS
  ( module X,
    -- * Handle
    Handle,
    newHandle,
    newHandleWithReplicaId,
    -- * Storage
    Storage,
    runStorage,
  )
where

import           RON.Prelude

import           Control.Exception (try)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Data.Bits (shiftL)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (find)
import           Data.Maybe (isJust)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory (canonicalizePath, createDirectoryIfMissing,
                                   doesDirectoryExist, doesPathExist,
                                   listDirectory, makeAbsolute, removeFile,
                                   renameDirectory)
import           System.FilePath (makeRelative, splitDirectories, (</>))
import           System.IO (hPutStrLn, stderr, withFile, IOMode(ReadMode))
import           System.IO.Error (isDoesNotExistError)
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error, throwErrorString)
import           RON.Event (OriginVariety (ApplicationSpecific), Replica,
                            ReplicaClock, advance, getEvents, getPid, mkReplica)
import           RON.Storage as X
import           RON.Storage.Backend (DocId (DocId), MonadStorage, RawDocId,
                                      changeDocId, deleteVersion,
                                      getCollections, getDocumentVersions,
                                      getDocuments, loadVersionContent,
                                      saveVersionContent)
import           RON.Util.Word (Word60, leastSignificant60)

newtype Storage a = Storage (ExceptT Error (ReaderT Handle EpochClock) a)
  deriving (Applicative, Functor, Monad, MonadError Error, MonadIO)

-- | Run a 'Storage' action
runStorage :: Handle -> Storage a -> IO a
runStorage h@Handle {replica, clock} (Storage action) = do
  res <-
    runEpochClock replica clock
      $ (`runReaderT` h)
      $ runExceptT action
  either throwIO pure res

instance MonadUnliftIO Storage where
  withRunInIO inner =
    Storage $
      ExceptT $
        ReaderT $ \handle ->
          liftIO $ try $ inner (runStorage handle)
  {-# INLINE withRunInIO #-}

instance ReplicaClock Storage where

  getPid = Storage . lift $ lift getPid

  getEvents = Storage . lift . lift . getEvents

  advance = Storage . lift . lift . advance

instance MonadStorage Storage where

  getCollections = Storage $ do
    Handle {dataDir} <- ask
    liftIO
      $ listDirectory dataDir
        >>= filterM (doesDirectoryExist . (dataDir </>))

  getDocuments :: forall doc. Collection doc => Storage [DocId doc]
  getDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

  getDocumentVersions = listDirectoryIfExists . docDir

  saveVersionContent docid version content = Storage $ do
    Handle {dataDir} <- ask
    let docdir = dataDir </> docDir docid
    liftIO $ do
      createDirectoryIfMissing True docdir
      atomicWriteFile (docdir </> version) content

  loadVersionContent docid version = Storage $ do
    Handle {dataDir} <- ask
    liftIO $ withFile (dataDir </> docDir docid </> version) ReadMode $
      fmap (BSL.fromChunks . (:[])) .  BS.hGetContents

  deleteVersion docid version = Storage $ do
    Handle {dataDir} <- ask
    liftIO
      $ do
        let file = dataDir </> docDir docid </> version
        removeFile file
        `catch` \e ->
          unless (isDoesNotExistError e) $ throwIO e

  changeDocId old new = Storage $ do
    Handle {dataDir} <- ask
    let oldPath = dataDir </> docDir old
        newPath = dataDir </> docDir new
    oldPathCanon <- liftIO $ canonicalizePath oldPath
    newPathCanon <- liftIO $ canonicalizePath newPath
    when (newPathCanon /= oldPathCanon) $ do
      newPathExists <- liftIO $ doesPathExist newPath
      when newPathExists
        $ throwErrorString
        $ unwords
            [ "changeDocId",
              show old,
              "[",
              oldPath,
              "->",
              oldPathCanon,
              "]",
              show new,
              "[",
              newPath,
              "->",
              newPathCanon,
              "]: internal error: new document id is already taken"
            ]
      liftIO $ renameDirectory oldPath newPath

-- | Storage handle (uses the “Handle pattern”).
data Handle
  = Handle
      { clock :: IORef Word60,
        dataDir :: FilePath,
        replica :: Replica
      }

-- | Create new storage handle.
-- Uses MAC address for replica id or generates a random one.
newHandle :: FilePath -> IO Handle
newHandle hDataDir = do
  macAddress <- getMacAddress
  replicaId <-
    case macAddress of
      Just macAddress' -> pure macAddress'
      Nothing -> fst . random <$> newTFGen
  newHandleWithReplicaId hDataDir $ leastSignificant60 replicaId

newHandleWithReplicaId :: FilePath -> Word60 -> IO Handle
newHandleWithReplicaId dataDir' replicaId = do
  dataDir <- makeAbsolute dataDir'
  time <- getCurrentEpochTime
  clock <- newIORef time
  let replica = mkReplica ApplicationSpecific replicaId
  pure Handle
    { clock,
      dataDir,
      replica
    }

listDirectoryIfExists :: FilePath -> Storage [FilePath]
listDirectoryIfExists relpath = Storage $ do
  Handle{dataDir} <- ask
  let dir = dataDir </> relpath
  liftIO $ do
    exists <- doesDirectoryExist dir
    if exists then listDirectory dir else pure []

docDir :: forall a. Collection a => DocId a -> FilePath
docDir (DocId dir) = collectionName @a </> dir

getMacAddress :: IO (Maybe Word64)
getMacAddress = do
  macAddress <- getMac
  pure $ decodeMac <$> macAddress
  where
    getMac = find (/= minBound) . map mac <$> getNetworkInterfaces
    decodeMac (MAC b5 b4 b3 b2 b1 b0) =
      (fromIntegral b5 `shiftL` 40)
        + (fromIntegral b4 `shiftL` 32)
        + (fromIntegral b3 `shiftL` 24)
        + (fromIntegral b2 `shiftL` 16)
        + (fromIntegral b1 `shiftL` 8)
        + fromIntegral b0
