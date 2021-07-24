{-# LANGUAGE NoImplicitPrelude #-}

module NetNode (startWorkers) where

import           Debug.Trace
import           RON.Prelude

import           Control.Monad (forever)
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=), (<?>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Network.WebSockets as WS
import qualified RON.Store as Store
import qualified RON.Store.Sqlite as Store
import           RON.Text.Parse (parseOpenFrame, parseUuid)
import           RON.Text.Serialize (serializeUuid)
import           RON.Text.Serialize.Experimental (serializeOpenFrame)
import           RON.Types.Experimental (Patch (..))

import           Fork (forkLinked)

startWorkers ::
  Store.Handle ->
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
startWorkers db listen peers = do
  for_ listen $ \port -> do
    traceM $ "Listening at [::]:" <> show port
    forkLinked $ WS.runServer "::" port server
  for_ peers  $ \port -> do
    traceM $ "Connecting to at [::]:" <> show port
    forkLinked $ WS.runClient "::" port "/" client
  where

    server pending = do
      conn <- WS.acceptRequest pending
      traceM $ "Accepted connection from " <> show (WS.pendingRequest pending)
      dialog db conn

    client = dialog db

dialog :: Store.Handle -> WS.Connection -> IO ()
dialog db conn = do
  -- advertise own database state
  -- TODO forkLinked $
  do
    patches <- Store.runStore db Store.loadLog
    if null patches then do
      traceM "No log for the chatroom"
    else do
      traceM $ "Log for the chatroom: " <> show (length patches)
      for_ patches \patch -> do
        traceM $ "Sending " <> show patch
        WS.sendBinaryData conn $ Aeson.encode $ NetPatch patch

  -- receive
  WS.withPingThread conn 30 (pure ()) $
    forever do
      messageData <- WS.receiveData conn
      case Aeson.eitherDecode messageData of
        Left e ->
          error $
            "NetNode.dialog: Aeson.eitherDecode: " <> e
            <> ", messageData = " <> show messageData
        Right netMessage -> do
          traceM $ "Received " <> show netMessage
          case netMessage of
            NetPatch patch -> Store.runStore db $ Store.appendPatch patch

newtype NetMessage = NetPatch Patch
  deriving Show

instance ToJSON NetMessage where
  toJSON = \case
    NetPatch Patch{object, log} ->
      Aeson.object
        [ "Type"   .= ("NetPatch" :: Text)
        , "object" .= TextL.decodeUtf8 (serializeUuid object)
        , "log"    .= TextL.decodeUtf8 (serializeOpenFrame log)
        ]

instance FromJSON NetMessage where
  parseJSON =
    Aeson.withObject "NetMessage" \o -> do
      type_ <- o .: "Type"
      case type_ of
        "NetPatch" -> do
          objectText <- TextL.encodeUtf8 <$> o .: "object"
          object <-
            either fail pure (parseUuid objectText) <?> Aeson.Key "object"
          logText <- TextL.encodeUtf8 <$> o .: "log"
          log <- either fail pure (parseOpenFrame logText) <?> Aeson.Key "log"
          pure $ NetPatch Patch{object, log}
        _ -> fail $ "unknown Type " <> type_
