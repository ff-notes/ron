{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Database (
    databaseToUIUpdater,
    loadAllMessages,
    messagePoster,
    newMessage,
    editMessageText,
) where

import RON.Prelude

import Control.Concurrent.STM (TChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.Logger (MonadLogger, logDebug)
import RON.Error (MonadE)
import RON.Event (ReplicaClock)
import RON.Experimental.Data.ORMap qualified as ORMap
import RON.Experimental.Data.ORSet (ORMap, ORSet)
import RON.Experimental.Data.ORSet qualified as ORSet
import RON.Store (MonadStore, newObject)
import RON.Store.Sqlite (fetchUpdates, runStore)
import RON.Store.Sqlite qualified as Store
import RON.Types (UUID)
import RON.Types.Experimental (Ref (..))
import RON.UUID qualified as UUID
import UnliftIO (MonadUnliftIO, atomically)

import Types (Message (..), MessageView (..), getMessageView)

loadAllMessages ::
    (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m [MessageView]
loadAllMessages db =
    runStore db do
        messageRefs <- ORSet.getDecode gMessageSetRef
        sortOn (.postTime) . catMaybes <$> for messageRefs getMessageView

newMessage ::
    (MonadE m, MonadStore m, ReplicaClock m) => Message -> m (Ref Message)
newMessage msg = do
    msgRef <- newObject msg
    ORSet.add_ gMessageSetRef msgRef
    pure msgRef

editMessageText ::
    (MonadStore m, ReplicaClock m) =>
    Ref Message -> Text -> m ()
editMessageText (Ref object) =
    ORMap.update (Ref @(ORMap Text Text) object) "text"

messagePoster ::
    (MonadLogger m, MonadUnliftIO m) => TChan Message -> Store.Handle -> m ()
messagePoster onMessagePosted db =
    forever do
        message <- atomically $ readTChan onMessagePosted
        $logDebug $ "Saving message " <> show message
        runStore db $ newMessage message

databaseToUIUpdater ::
    (MonadLogger m, MonadUnliftIO m) =>
    Store.Handle ->
    TChan [MessageView] ->
    m ()
databaseToUIUpdater db onMessageListUpdated = do
    onUpdate <- fetchUpdates db
    forever do
        _ <- atomically $ readTChan onUpdate
        messages <- loadAllMessages db
        atomically $ writeTChan onMessageListUpdated messages

gMessageSetUuid :: UUID
gMessageSetUuid = $(UUID.liftName "messages")

gMessageSetRef :: Ref (ORSet (Ref Message))
gMessageSetRef = Ref gMessageSetUuid
