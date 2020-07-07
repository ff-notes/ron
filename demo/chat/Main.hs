import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTChanIO)
import           RON.Store.FS (runStore)
import qualified RON.Store.FS as Store
import           Text.Pretty.Simple (pPrint)

import qualified Database
import qualified HttpNode
import           Options (Command (Post, Show, UI), UIOptions (UIOptions),
                          parseCommand)
import qualified Options
import           Types (Env (Env), MessageContent (MessageContent))
import qualified Types
import           UI (runUI)

main :: IO ()
main = do
  cmd <- parseCommand
  mDb <- Store.newHandle "./data"
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
      onMessagePosted      <- newTChanIO
      onMessageListUpdated <- newTChanIO
      let env = Env{username, onMessagePosted, onMessageListUpdated}
      _ <- forkIO $ Database.databaseUpdateWorker db onMessageListUpdated
      _ <- forkIO $ Database.messagePostWorker onMessagePosted db
      _ <- forkIO $ HttpNode.worker listen peers
      runUI db env
