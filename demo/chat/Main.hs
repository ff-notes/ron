import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTChanIO)
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           RON.Store.FS (runStore)
import qualified RON.Store.FS as Store
import           Text.Pretty.Simple (pPrint)

import qualified Database
import qualified NetNode
import           Options (Command (Post, Show, UI), Options (Options),
                          UIOptions (UIOptions), parseOptions)
import qualified Options
import           Types (Env (Env), MessageContent (MessageContent))
import qualified Types
import           UI (runUI)

main :: IO ()
main = do
  Options{dataDir, cmd} <- parseOptions
  mDb <- Store.newHandle dataDir
  db <-
    case mDb of
      Nothing -> fail "Application is already running"
      Just db -> pure db
  case cmd of
    Show -> Database.loadAllMessages db >>= pPrint
    Post username text -> do
      messageRef <-
        runStore db $ Database.newMessage MessageContent{username, text}
      putStrLn $ "created message: " <> show messageRef
    UI UIOptions{username, listen, peers} -> do
      when (isNothing listen && null peers) $
        fail
          "The peer must connect to other peers or listen for connections. \
          \Specify `--listen` or `--peer`."
      onMessagePosted      <- newTChanIO
      onMessageListUpdated <- newTChanIO
      let env = Env{username, onMessagePosted, onMessageListUpdated}
      _ <- forkIO $ Database.databaseToUIUpdater db onMessageListUpdated
      _ <- forkIO $ Database.messagePoster onMessagePosted db
      NetNode.startWorkers db listen peers
      runUI db env
