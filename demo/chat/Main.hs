import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM (newTChanIO)
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store
import           Text.Pretty.Simple (pPrint)

import qualified Database
import           Fork (forkLinked)
import qualified NetNode
import           Options (Command (Post, RunNode, RunUI, Show),
                          NodeOptions (..), Options (..), UIOptions (..),
                          parseOptions)
import           Types (Env (..), MessageContent (..))
import           UI (initUI, runUI)

main :: IO ()
main = do
  Options{dataDir, cmd} <- parseOptions
  db <- Store.newHandle dataDir
  case cmd of
    Show -> Database.loadAllMessages db >>= pPrint
    Post username text -> do
      messageRef <-
        runStore db $ Database.newMessage MessageContent{username, text}
      putStrLn $ "created message: " <> show messageRef
    RunNode nodeOptions -> runNode db nodeOptions
    RunUI UIOptions{username} nodeOptions -> do
      async $ runNode db nodeOptions
      onMessagePosted      <- newTChanIO
      onMessageListUpdated <- newTChanIO
      let env = Env{username, onMessagePosted, onMessageListUpdated}
      uiHandle <- initUI db env
      forkLinked $ Database.databaseToUIUpdater db onMessageListUpdated
      forkLinked $ Database.messagePoster onMessagePosted db
      runUI uiHandle

runNode :: Store.Handle -> NodeOptions -> IO ()
runNode db NodeOptions{listen, peers} = do
  when (isNothing listen && null peers) $
    fail
      "The peer must connect to other peers or listen for connections. \
      \Specify `--listen` or `--peer`."
  NetNode.workers db listen peers
