import           Control.Monad (when)
import           Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Tree.View (drawTree)
import qualified RON.Data.GTree as GTree
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store (Handle, newHandle)
import           RON.Text.Serialize (serializeUuid)
import           UnliftIO (MonadUnliftIO, liftIO)

import           Database (loadTheTree, theTreeRef)
import           Fork (forkLinked)
import qualified NetNode
import           Options (Command (Add, RunNode, RunUI, Show), NodeOptions (..),
                          Options (..), parseOptions)
import           UI (runUI)

main :: IO ()
main = do
  Options{database, cmd} <- parseOptions
  runStderrLoggingT do
    db <- Store.newHandle database
    case cmd of
      Show -> do
        tree <- loadTheTree db
        liftIO $ drawTree $ BSLC.unpack . serializeUuid <$> tree
      Add parent -> runStore db $ GTree.insert theTreeRef parent
      RunNode nodeOptions -> runNode db nodeOptions
      RunUI nodeOptions -> do
        forkLinked $ runNode db nodeOptions
        runUI db

runNode ::
  (MonadFail m, MonadLogger m, MonadUnliftIO m) =>
  Store.Handle -> NodeOptions -> m ()
runNode db options@NodeOptions{listenPorts, peers} = do
  when (null listenPorts && null peers) $
    fail
      "The peer must connect to other peers or listen for connections. \
      \Specify `--listen` or `--peer`."
  NetNode.workers db options
