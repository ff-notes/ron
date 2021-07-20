{-# LANGUAGE NoImplicitPrelude #-}

module Database
  ( chatroomUuid
  , databaseToUIUpdater
  , loadAllMessages
  , messagePoster
  , newMessage
  ) where

import           RON.Prelude

import           Control.Concurrent.STM (TChan, atomically, readTChan,
                                         writeTChan)
import           Control.Monad (forever)
import           RON.Data.ORSet.Experimental (ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Store (MonadStore, newObject, readObject)
import           RON.Store.FS (runStore)
import qualified RON.Store.FS as Store
import           RON.Types (Atom (AString, AUuid), UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID
import           System.IO (putStrLn)

import           Types (Env (..), MessageContent (..), MessageView, postTime)

loadAllMessages :: Store.Handle -> IO [MessageView]
loadAllMessages db =
  runStore db $ do
    mMessageSet <- readObject gMessageSetRef
    case mMessageSet of
      Nothing -> do
        liftIO $ putStrLn "!!! messages collection doesn't exist !!!"
        pure []
      Just messageSet -> do
        messageRefs <- ORSet.toList messageSet
        sortOn postTime . catMaybes <$> for messageRefs readObject

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  MessageContent -> m (Ref MessageView)
newMessage MessageContent{username, text} = do
  msgRef <- newObject @MessageView
  ORSet.add_ msgRef ("username", [AString username])
  ORSet.add_ msgRef ("text",     [AString text    ])
  ORSet.add_ gMessageSetRef msgRef
  pure msgRef

messagePoster :: TChan MessageContent -> Store.Handle -> Env -> IO ()
messagePoster onMessagePosted db Env{putLog} =
  forever $ do
    message <- atomically $ readTChan onMessagePosted
    putLog $ "Saving message " <> show message
    runStore db $ newMessage message

databaseToUIUpdater :: Store.Handle -> TChan [MessageView] -> IO ()
databaseToUIUpdater db onMessageListUpdated = do
  Store.subcribeToObject db chatroomUuid
  onObjectChanged <- Store.fetchUpdates db
  forever $ do
    objectId <- atomically $ readTChan onObjectChanged
    when (objectId == chatroomUuid) $ do
      messages <- loadAllMessages db
      atomically $ writeTChan onMessageListUpdated messages
    -- ignore other changes

chatroomUuid :: UUID
chatroomUuid = $(UUID.liftName "chatroom")

gMessageSetRef :: Ref (ORSet (Ref MessageView))
gMessageSetRef = Ref chatroomUuid [AUuid $(UUID.liftName "message")]
