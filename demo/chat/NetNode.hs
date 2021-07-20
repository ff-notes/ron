{-# LANGUAGE NoImplicitPrelude #-}

module NetNode (startWorkers) where

import           RON.Prelude

import           Control.Concurrent (threadDelay)
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=), (<?>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Network.WebSockets as WS
import qualified RON.Store as Store
import qualified RON.Store.FS as Store
import           RON.Text.Parse (parseOpenFrame, parseUuid)
import           RON.Text.Serialize (serializeUuid)
import           RON.Text.Serialize.Experimental (serializeOpenFrame)
import           RON.Types (OpenFrame, UUID)

import           Database (chatroomUuid)
import           Fork (fork)
import           Types (Env (Env, putLog))

startWorkers ::
  Store.Handle ->
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  Env ->
  IO ()
startWorkers db listen peers env@Env{putLog} = do
  for_ listen $ \port -> do
    putLog $ "Listening at [::]:" <> show port
    fork $ WS.runServer "::" port server
  for_ peers  $ \port -> do
    putLog $ "Connecting to at [::]:" <> show port
    fork $ WS.runClient "::" port "/" client
  where

    server pending = do
      conn <- WS.acceptRequest pending
      putLog $ "Accepted connection from " <> show (WS.pendingRequest pending)
      dialog db env conn

    client = dialog db env

dialog :: Store.Handle -> Env -> WS.Connection -> IO ()
dialog db Env{putLog} conn = do
  -- advertise own chatroom state and interesting object update requests
  -- TODO fork $
  do
    ops <-
      fmap fold $ Store.runStore db $ Store.loadObjectLog chatroomUuid mempty
    let netMessage = ObjectOps chatroomUuid ops
    if null ops then do
      putLog "No ops for chatroom"
    else do
      putLog $ "Log for chatroom " <> show netMessage
      WS.sendBinaryData conn $ Aeson.encode netMessage
  do
    objectSubscriptions <- Store.readObjectSubscriptions db
    for_ objectSubscriptions $ \object -> do
      let request = RequestObjectChanges object
      putLog $ "For object substription, sending " <> show request
      WS.sendBinaryData conn $ Aeson.encode request
    threadDelay 1_000_000

  -- receive
  WS.withPingThread conn 30 (pure ()) $ do
    messageData <- WS.receiveData conn
    case Aeson.eitherDecode messageData of
      Left e ->
        error $
          "NetNode.dialog: Aeson.eitherDecode: " <> e
          <> ", messageData = " <> show messageData
      Right netMessage -> do
        putLog $ "Received " <> show netMessage
        case netMessage of
          ObjectOps object ops ->
            Store.runStore db $ Store.appendPatches object ops
          RequestObjectChanges object -> do
            ops <-
              fmap fold $ Store.runStore db $ Store.loadObjectLog object mempty
            let response = ObjectOps object ops
            if null ops then do
              putLog $
                "In response to changes request, there's no ops " <> show object
            else do
              putLog $
                "In response to changes request, sending " <> show response
              WS.sendBinaryData conn $ Aeson.encode response
    pure ()

data NetMessage
  = ObjectOps UUID OpenFrame
  | RequestObjectChanges UUID
  deriving Show

instance ToJSON NetMessage where
  toJSON = \case
    ObjectOps object ops ->
      Aeson.object
        [ "Type"   .= ("ObjectOps" :: Text)
        , "object" .= TextL.decodeUtf8 (serializeUuid object)
        , "ops"    .= TextL.decodeUtf8 (serializeOpenFrame ops)
        ]
    RequestObjectChanges object ->
      Aeson.object
        [ "Type"   .= ("RequestObjectChanges" :: Text)
        , "object" .= TextL.decodeUtf8 (serializeUuid object)
        ]

instance FromJSON NetMessage where
  parseJSON =
    Aeson.withObject "NetMessage" \o -> do
      type_ <- o .: "Type"
      case type_ of
        "ObjectOps" -> do
          objectText <- TextL.encodeUtf8 <$> o .: "object"
          object <-
            either fail pure (parseUuid objectText) <?> Aeson.Key "object"
          opsText <- TextL.encodeUtf8 <$> o .: "ops"
          ops <- either fail pure (parseOpenFrame opsText) <?> Aeson.Key "ops"
          pure $ ObjectOps object ops
        "RequestObjectChanges" -> do
          objectText <- TextL.encodeUtf8 <$> o .: "object"
          object <-
            either fail pure (parseUuid objectText) <?> Aeson.Key "object"
          pure $ RequestObjectChanges object
        _ -> fail $ "unknown Type " <> type_
