module NetNode (worker) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Extra (void, whenJust)
import           Data.Foldable (for_)
import qualified Network.WebSockets as WS
import           RON.Text.Parse (parseOpenFrame)
import           RON.Types (OpenFrame)

worker ::
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
worker listen peers = do
  whenJust listen $ \myPort ->
    void $ forkIO $ WS.runServer "::" myPort serverApp
  for_ peers $ \theirPort -> WS.runClient "::" theirPort "/" clientApp

serverApp :: WS.ServerApp
serverApp pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  handleDuplexConnection conn

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

clientApp :: WS.ClientApp ()
clientApp = handleDuplexConnection
