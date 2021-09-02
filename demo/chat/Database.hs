module Database
  ( chatroomUuid
  , databaseToUIUpdater
  , loadAllMessages
  , messagePoster
  , newMessage
  ) where

import Prelude hiding (show)
import RON.Prelude

import Control.Concurrent.STM (TChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.Logger (MonadLogger, logDebug)
import RON.Error (MonadE)
import RON.Event (ReplicaClock)
import RON.Experimental.Data (castRepr)
import RON.Experimental.Data.ORSet (ORSet)
import RON.Experimental.Data.ORSet qualified as ORSet
import RON.Store (MonadStore, newObject)
import RON.Store.Sqlite (fetchUpdates, runStore)
import RON.Store.Sqlite qualified as Store
import RON.Types (Atom (AUuid), UUID)
import RON.Types.Experimental (Ref (..))
import RON.UUID qualified as UUID
import UnliftIO (MonadUnliftIO, atomically)

import Types (Message (..), MessageView, getMessageView, postTime)

loadAllMessages ::
  (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m [MessageView]
loadAllMessages db =
  runStore db do
    messageRefs <- ORSet.getDecode gMessageSetRef
    sortOn postTime . catMaybes <$> for messageRefs getMessageView

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) => Message -> m (Ref Message)
newMessage Message{username, text} = do
  msgRef <- newObject @Message
  let msgRepRef = castRepr msgRef
  ORSet.add_ msgRepRef ("username", username)
  ORSet.add_ msgRepRef ("text",     text)
  ORSet.add_ gMessageSetRef msgRef
  pure msgRef

messagePoster ::
  (MonadLogger m, MonadUnliftIO m) => TChan Message -> Store.Handle -> m ()
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

gMessageSetRef :: Ref (ORSet (Ref Message))
gMessageSetRef = Ref chatroomUuid [AUuid $(UUID.liftName "message")]
