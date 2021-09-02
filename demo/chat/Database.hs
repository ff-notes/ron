{-# LANGUAGE NoImplicitPrelude #-}

module Database
  ( chatroomUuid
  , databaseToUIUpdater
  , loadAllMessages
  , messagePoster
  , newMessage
  ) where

import RON.Prelude

import Control.Concurrent.STM (TChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.Logger (MonadLogger, logDebug)
import RON.Data.Experimental (castRepr)
import RON.Data.ORSet.Experimental (ORSet)
import RON.Data.ORSet.Experimental qualified as ORSet
import RON.Error (MonadE)
import RON.Event (ReplicaClock)
import RON.Store (MonadStore, newObject, readObject)
import RON.Store.Sqlite (fetchUpdates, runStore)
import RON.Store.Sqlite qualified as Store
import RON.Types (Atom (AUuid), UUID)
import RON.Types.Experimental (Ref (..))
import RON.UUID qualified as UUID
import UnliftIO (MonadUnliftIO, atomically)

import Types (MessageContent (..), MessageView, postTime)

loadAllMessages ::
  (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m [MessageView]
loadAllMessages db =
  runStore db do
    messageRefs <- ORSet.getDecode gMessageSetRef
    sortOn postTime . catMaybes <$> for messageRefs readObject

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  MessageContent -> m (Ref MessageView)
newMessage MessageContent{username, text} = do
  msgRef <- newObject @MessageView
  let msgRepRef = castRepr msgRef
  ORSet.add_ msgRepRef ("username", username)
  ORSet.add_ msgRepRef ("text",     text)
  ORSet.add_ gMessageSetRef msgRef
  pure msgRef

messagePoster ::
  (MonadLogger m, MonadUnliftIO m) =>
  TChan MessageContent -> Store.Handle -> m ()
messagePoster onMessagePosted db =
  forever $ do
    message <- atomically $ readTChan onMessagePosted
    $logDebug $ "Saving message " <> show message
    runStore db $ newMessage message

databaseToUIUpdater ::
  (MonadLogger m, MonadUnliftIO m) =>
  Store.Handle -> TChan [MessageView] -> m ()
databaseToUIUpdater db onMessageListUpdated = do
  onUpdate <- fetchUpdates db
  forever $ do
    _ <- atomically $ readTChan onUpdate
    messages <- loadAllMessages db
    atomically $ writeTChan onMessageListUpdated messages

chatroomUuid :: UUID
chatroomUuid = $(UUID.liftName "chatroom")

gMessageSetRef :: Ref (ORSet (Ref MessageView))
gMessageSetRef = Ref chatroomUuid [AUuid $(UUID.liftName "message")]
