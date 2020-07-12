module NetNode (worker) where

import           Control.Monad.Extra (whenJust)
import qualified Network.WebSockets as WS
import           RON.Text.Parse (parseOpenFrame)
import           RON.Types (OpenFrame)

worker ::
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
worker listen _peers =
  whenJust listen $ \port ->
    WS.runServer "::" port serverApp

serverApp :: WS.ServerApp
serverApp pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  WS.withPingThread conn 30 (pure ()) $ do
    frameData <- WS.receiveData conn
    case parseOpenFrame frameData of
      Left e -> error e
      Right frame -> handleIncomingFrame frame
    pure ()

handleIncomingFrame :: OpenFrame -> IO ()
handleIncomingFrame = undefined
