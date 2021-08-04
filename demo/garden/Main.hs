import Control.Monad (when)
import Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Tree.View (drawTree)
import RON.Data.GTree qualified as GTree
import RON.Store.Sqlite (runStore)
import RON.Store.Sqlite qualified as Store (Handle, newHandle)
import RON.Text.Serialize (serializeUuid)
import UnliftIO (MonadUnliftIO, liftIO)

import Database (loadTheTree, theTreeRef)
import Fork (forkLinked)
import NetNode qualified
import Options (Command (Add, RunNode, RunUI, Show), NodeOptions (..),
                Options (..), parseOptions)
import UI (runUI)

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
