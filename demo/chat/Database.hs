{-# LANGUAGE NoImplicitPrelude #-}

module Database
  ( chatroomUuid
  , databaseToUIUpdater
  , loadAllMessages
  , messagePoster
  , newMessage
  ) where

import           Debug.Trace
import           RON.Prelude

import           Control.Concurrent.STM (TChan, atomically, readTChan,
                                         writeTChan)
import           Control.Monad (forever)
import           RON.Data.ORSet.Experimental (ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Store (MonadStore, newObject, readObject)
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store
import           RON.Types (Atom (AString, AUuid), UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID

import           Types (MessageContent (..), MessageView, postTime)

loadAllMessages :: Store.Handle -> IO [MessageView]
loadAllMessages db =
  runStore db $ do
    mMessageSet <- readObject gMessageSetRef
    case mMessageSet of
      Nothing ->
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

messagePoster :: TChan MessageContent -> Store.Handle -> IO ()
messagePoster onMessagePosted db =
  forever $ do
    message <- atomically $ readTChan onMessagePosted
    traceM $ "Saving message " <> show message
    runStore db $ newMessage message

databaseToUIUpdater :: Store.Handle -> TChan [MessageView] -> IO ()
databaseToUIUpdater db onMessageListUpdated = do
  onUpdate <- Store.fetchUpdates db
  forever $ do
    _ <- atomically $ readTChan onUpdate
    messages <- loadAllMessages db
    atomically $ writeTChan onMessageListUpdated messages

chatroomUuid :: UUID
chatroomUuid = $(UUID.liftName "chatroom")

gMessageSetRef :: Ref (ORSet (Ref MessageView))
gMessageSetRef = Ref chatroomUuid [AUuid $(UUID.liftName "message")]
