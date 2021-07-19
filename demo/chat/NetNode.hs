{-# LANGUAGE NoImplicitPrelude #-}

module NetNode (startWorkers) where

import           RON.Prelude

import           Control.Concurrent (threadDelay)
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Network.WebSockets as WS
import qualified RON.Store.FS as Store
import           RON.Text.Parse (parseOpenFrame, parseUuid)
import           RON.Text.Serialize (serializeOp, serializeUuid)
import           RON.Types (OpenFrame, UUID)

import           Fork (fork)

startWorkers ::
  Store.Handle ->
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
startWorkers db listen peers =
  do
    for_ listen $ \port -> fork $ WS.runServer "::" port     server
    for_ peers  $ \port -> fork $ WS.runClient "::" port "/" client
  where
    server = WS.acceptRequest >=> dialog db
    client = dialog db

dialog :: Store.Handle -> WS.Connection -> IO ()
dialog db conn = do
  -- send object update requests
  -- fork $
  do
    objectSubscriptions <- Store.readObjectSubscriptions db
    for_ objectSubscriptions $ \object ->
      WS.sendBinaryData conn $ Aeson.encode $ RequestObjectChanges object
    threadDelay 1_000_000

  -- receive
  WS.withPingThread conn 30 (pure ()) $ do
    messageData <- WS.receiveData conn
    case Aeson.eitherDecode messageData of
      Left e -> error $ "NetNode.dialog: Aeson.eitherDecode: " <> e <> ", messageData = " <> show messageData
      Right Ops{} -> do
        -- TODO incorporate ops
        undefined
      Right (RequestObjectChanges object) -> do
        ops <- fmap fold $ Store.runStore db $ Store.loadObjectLog object mempty
        unless (null ops) $
          WS.sendBinaryData conn $ Aeson.encode $ Ops ops
    pure ()

data NetMessage
  = Ops OpenFrame
  | RequestObjectChanges UUID

instance ToJSON NetMessage where
  toJSON = \case
    Ops ops ->
      Aeson.object
        [ "Type" .= ("Ops" :: Text)
        , "ops"  .= map (TextL.decodeUtf8 . serializeOp) ops
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
        "Ops" -> do
          opsText <- TextL.encodeUtf8 <$> o .: "object"
          ops <- either fail pure $ parseOpenFrame opsText
          pure $ Ops ops
        "RequestObjectChanges" -> do
          objectText <- TextL.encodeUtf8 <$> o .: "object"
          object <- either fail pure $ parseUuid objectText
          pure $ RequestObjectChanges object
        _ -> fail $ "unknown Type " <> type_
