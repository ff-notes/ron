module NetNode (startWorkers) where

import           Control.Concurrent (forkIO)
import           Control.Monad ((>=>))
import           Data.Foldable (for_)
import qualified Network.WebSockets as WS
import           RON.Text.Parse (parseOpenFrame)
import           RON.Types (OpenFrame)

startWorkers ::
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
startWorkers listen peers =
  do
    for_ listen $ \port -> forkIO $ WS.runServer "::" port     server
    for_ peers  $ \port -> forkIO $ WS.runClient "::" port "/" client
  where
    server = WS.acceptRequest >=> handleDuplexConnection
    client = handleDuplexConnection

handleDuplexConnection :: WS.Connection -> IO ()
handleDuplexConnection conn = do
  WS.withPingThread conn 30 (pure ()) $ do
    frameData <- WS.receiveData conn
    case parseOpenFrame frameData of
      Left e -> error e
      Right frame -> handleIncomingFrame frame
    pure ()

handleIncomingFrame :: OpenFrame -> IO ()
handleIncomingFrame = undefined
