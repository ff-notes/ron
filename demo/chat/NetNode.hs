module NetNode (startWorkers) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad ((>=>))
import           Data.Foldable (for_)
import qualified Network.WebSockets as WS
import           RON.Prelude (ByteStringL)
import qualified RON.Store.FS as Store
import           RON.Text.Parse (parseOpenFrame)
import           RON.Types (OpenFrame, UUID)

startWorkers ::
  Store.Handle ->
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
startWorkers db listen peers =
  do
    for_ listen $ \port -> forkIO $ WS.runServer "::" port     server
    for_ peers  $ \port -> forkIO $ WS.runClient "::" port "/" client
  where
    server = WS.acceptRequest >=> dialog db
    client = dialog db

dialog :: Store.Handle -> WS.Connection -> IO ()
dialog db conn = do
  -- send object update requests
  _ <-
    forkIO $ do
      objectSubscriptions <- Store.readObjectSubscriptions db
      for_ objectSubscriptions $ \object ->
        WS.sendBinaryData conn $ encodeNetMessage $ RequestChanges{object}
      threadDelay 1_000_000

  -- receive
  WS.withPingThread conn 30 (pure ()) $ do
    frameData <- WS.receiveData conn
    case parseOpenFrame frameData of
      Left e      -> error e
      Right frame -> handleIncomingFrame frame
    pure ()

handleIncomingFrame :: OpenFrame -> IO ()
handleIncomingFrame = undefined

newtype NetMessage = RequestChanges{object :: UUID}

encodeNetMessage :: NetMessage -> ByteStringL
encodeNetMessage = undefined
