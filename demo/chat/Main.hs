import           Control.Concurrent.STM (newTChanIO)
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store
import           Text.Pretty.Simple (pPrint)

import qualified Database
import           Fork (forkLinked)
import qualified NetNode
import           Options (Command (Post, Show, UI), Options (Options),
                          UIOptions (UIOptions), parseOptions)
import qualified Options
import           Types (Env (Env), MessageContent (MessageContent))
import qualified Types
import           UI (initUI, runUI)

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
      let env0 =
            Env
              { username
              , onMessagePosted
              , onMessageListUpdated
              , putLog = undefined
              }
      (uiHandle, env) <- initUI db env0
      forkLinked $ Database.databaseToUIUpdater db onMessageListUpdated
      forkLinked $ Database.messagePoster onMessagePosted db env
      NetNode.startWorkers db listen peers env
      runUI uiHandle
