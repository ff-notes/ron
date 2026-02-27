{-# LANGUAGE NoImplicitPrelude #-}

module NetNode (workers) where

import RON.Prelude

import Control.Concurrent.STM (readTChan)
import Control.Monad (forever)
import Control.Monad.Logger (MonadLogger, logInfo)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=), (<?>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text.Lazy.Encoding qualified as TextL
import Network.WebSockets qualified as WS
import RON.Store (appendPatch)
import RON.Store.Sqlite (fetchUpdates, loadOpLog, runStore)
import RON.Store.Sqlite qualified as Store (Handle)
import RON.Text.Parse (parseOpenFrame, parseUuid)
import RON.Text.Serialize (serializeUuid)
import RON.Text.Serialize.Experimental (serializeOpenFrame)
import RON.Types.Experimental (Patch (..))
import UnliftIO (
    MonadUnliftIO,
    atomically,
    concurrently_,
    forConcurrently_,
    liftIOOp,
    withRunInIO,
 )

import Fork (forkLinked)
import Options (NodeOptions (..), Peer (..))

-- Like 'liftIOOp :: MonadUnliftIO m => (IO a -> IO b) -> m a -> m b',
-- but with an extra argument for the inner function.
liftIOOp' :: (MonadUnliftIO m) => ((x -> IO a) -> IO b) -> (x -> m a) -> m b
liftIOOp' f g = withRunInIO \unlift -> f $ unlift . g

workers ::
    (MonadLogger m, MonadUnliftIO m) => Store.Handle -> NodeOptions -> m ()
workers db NodeOptions{listenHost, listenPorts, peers} =
    concurrently_ runServers runClients
  where
    runServers =
        forConcurrently_ listenPorts \port -> do
            $logInfo $ "Listening at [" <> show listenHost <> "]:" <> show port
            liftIOOp' (WS.runServer (show listenHost) port) server

    runClients =
        forConcurrently_ peers \peer@Peer{host, port} -> do
            $logInfo $ "Connecting to " <> show peer
            liftIOOp' (WS.runClient host port "/") client

    server pending = do
        conn <- liftIO $ WS.acceptRequest pending
        $logInfo $ "Accepted connection " <> show (WS.pendingRequest pending)
        dialog db conn

    client = dialog db

dialog ::
    (MonadLogger m, MonadUnliftIO m) => Store.Handle -> WS.Connection -> m ()
dialog db conn = do
    -- first, advertise own database state
    do
        patches <- runStore db loadOpLog
        case patches of
            [] -> $logInfo "No log for the chatroom"
            _ : _ -> do
                $logInfo $ "Log for the chatroom: " <> show (length patches)
                for_ patches \patch -> do
                    $logInfo $ "Send initial patch " <> show patch
                    liftIO
                        . WS.sendBinaryData conn
                        . Aeson.encode
                        $ NetPatch patch

    -- send
    forkLinked do
        onUpdate <- fetchUpdates db
        forever $ do
            patch <- atomically $ readTChan onUpdate
            $logInfo $ "Send new patch " <> show patch
            liftIO $ WS.sendBinaryData conn $ Aeson.encode $ NetPatch patch

    -- receive
    liftIOOp (WS.withPingThread conn 30 (pure ()))
        $ forever do
            messageData <- liftIO $ WS.receiveData conn
            case Aeson.eitherDecode messageData of
                Left e ->
                    error
                        $ "NetNode.dialog: Aeson.eitherDecode: "
                        <> e
                        <> ", messageData = "
                        <> show messageData
                Right netMessage -> do
                    $logInfo $ "Received " <> show netMessage
                    case netMessage of
                        NetPatch patch -> runStore db $ appendPatch patch

newtype NetMessage = NetPatch Patch
    deriving (Show)

instance ToJSON NetMessage where
    toJSON = \case
        NetPatch Patch{object, log} ->
            Aeson.object
                [ "Type" .= ("NetPatch" :: Text)
                , "object" .= TextL.decodeUtf8 (serializeUuid object)
                , "log" .= TextL.decodeUtf8 (serializeOpenFrame $ toList log)
                ]

instance FromJSON NetMessage where
    parseJSON =
        Aeson.withObject "NetMessage" \o -> do
            type_ <- o .: "Type"
            case type_ of
                "NetPatch" -> do
                    objectText <- TextL.encodeUtf8 <$> o .: "object"
                    object <-
                        either fail pure (parseUuid objectText)
                            <?> Aeson.Key "object"
                    logText <- TextL.encodeUtf8 <$> o .: "log"
                    logList <-
                        either fail pure (parseOpenFrame logText)
                            <?> Aeson.Key "log"
                    log <- maybe (fail "empty log") pure $ nonEmpty logList
                    pure $ NetPatch Patch{object, log}
                _ -> fail $ "unknown Type " <> type_
