{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
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
    watch
    )
where

import Control.Concurrent.STM
  ( TChan,
    atomically,
    dupTChan,
    newBroadcastTChanIO,
    readTChan,
    writeTChan
    )
import Control.Monad (forever)
import Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import RON.Error (Error, throwErrorString)
import RON.Event
  ( EpochTime,
    ReplicaClock,
    ReplicaId,
    advance,
    applicationSpecific,
    getEvents,
    getPid
    )
import RON.Prelude
import RON.Storage as X
import RON.Storage.Backend
  ( DocId (DocId),
    MonadStorage,
    changeDocId,
    deleteVersion,
    getCollections,
    getDocumentVersions,
    getDocuments,
    loadVersionContent,
    saveVersionContent
    )
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    listDirectory,
    removeFile,
    renameDirectory
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Random.TF (newTFGen)
import System.Random.TF.Instances (random)

-- | Environment is the dataDir
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

  saveVersionContent docid version content = do
    Storage $ do
      Handle {dataDir} <- ask
      let docdir = dataDir </> docDir docid
      liftIO $ do
        createDirectoryIfMissing True docdir
        BSL.writeFile (docdir </> version) content
    emitDocumentChanged docid

  loadVersionContent docid version = Storage $ do
    Handle {dataDir} <- ask
    liftIO $ BSL.readFile $ dataDir </> docDir docid </> version

  deleteVersion docid version = Storage $ do
    Handle {dataDir} <- ask
    liftIO $ do
      let file = dataDir </> docDir docid </> version
      removeFile file
      `catch` \e ->
        unless (isDoesNotExistError e) $ throwIO e

  changeDocId old new = do
    renamed <- x
    when renamed $ emitDocumentChanged new
    where
      x = Storage $ do
        Handle {dataDir} <- ask
        let oldPath = dataDir </> docDir old
            newPath = dataDir </> docDir new
        oldPathCanon <- liftIO $ canonicalizePath oldPath
        newPathCanon <- liftIO $ canonicalizePath newPath
        let pathsDiffer = newPathCanon /= oldPathCanon
        when pathsDiffer $ do
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
        pure pathsDiffer

-- | Storage handle (uses the “Handle pattern”).
data Handle
  = Handle
      { clock :: IORef EpochTime,
        dataDir :: FilePath,
        replica :: ReplicaId,
        onDocumentChanged :: TChan CollectionDocId
        }

emitDocumentChanged :: Collection a => DocId a -> Storage ()
emitDocumentChanged docid = Storage $ do
  Handle {onDocumentChanged} <- ask
  liftIO . atomically $ writeTChan onDocumentChanged $ CollectionDocId docid

-- | Create new storage handle.
-- Uses MAC address for replica id or generates a random one.
newHandle :: FilePath -> IO Handle
newHandle hDataDir = do
  macAddress <- getMacAddress
  replicaId <-
    case macAddress of
      Just macAddress' -> pure macAddress'
      Nothing -> fst . random <$> newTFGen
  newHandleWithReplicaId hDataDir replicaId

newHandleWithReplicaId :: FilePath -> Word64 -> IO Handle
newHandleWithReplicaId dataDir replicaId = do
  time <- getCurrentEpochTime
  clock <- newIORef time
  let replica = applicationSpecific replicaId
  onDocumentChanged <- newBroadcastTChanIO
  pure Handle {dataDir, clock, replica, onDocumentChanged}

listDirectoryIfExists :: FilePath -> Storage [FilePath]
listDirectoryIfExists relpath = Storage $ do
  Handle {dataDir} <- ask
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
    getMac =
      listToMaybe . filter (/= minBound) . map mac <$> getNetworkInterfaces
    decodeMac (MAC b5 b4 b3 b2 b1 b0) =
      (fromIntegral b5 `shiftL` 40)
        + (fromIntegral b4 `shiftL` 32)
        + (fromIntegral b3 `shiftL` 24)
        + (fromIntegral b2 `shiftL` 16)
        + (fromIntegral b1 `shiftL` 8)
        + fromIntegral b0

{- |
  Watch for changes,
  calling the action each time a document changes inside a replica or outside.

  This function blocks its thread.
  -}
watch
  :: Handle
  -> (CollectionDocId -> IO ()) -- ^ action
  -> IO ()
watch Handle {onDocumentChanged} action = do
  childChan <- atomically $ dupTChan onDocumentChanged
  forever $ do
    docId <- atomically $ readTChan childChan
    action docId
