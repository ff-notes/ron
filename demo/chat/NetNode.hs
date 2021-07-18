module NetNode (startWorkers) where

import           RON.Prelude

import           Control.Concurrent (threadDelay)
import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Network.WebSockets as WS
import qualified RON.Store.FS as Store
import           RON.Text.Parse (parseOpenFrame)
import           RON.Text.Serialize (serializeUuid)
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
      WS.sendBinaryData conn $ Aeson.encode RequestChanges{object}
    threadDelay 1_000_000

  -- receive
  WS.withPingThread conn 30 (pure ()) $ do
    frameData <- WS.receiveData conn
    case parseOpenFrame frameData of
      Left e      -> error $ "NetNode.dialog: parseOpenFrame: " <> e
      Right frame -> handleIncomingFrame frame
    pure ()

handleIncomingFrame :: OpenFrame -> IO ()
handleIncomingFrame _ = error "undefined handleIncomingFrame"

newtype NetMessage = RequestChanges{object :: UUID}

instance ToJSON NetMessage where
  toJSON RequestChanges{object} =
    Aeson.object
      [ "Type" .= ("RequestChanges" :: Text)
      , "object" .= TextL.decodeUtf8 (serializeUuid object)
      ]
