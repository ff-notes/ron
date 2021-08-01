import           Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Tree (Tree (Node))
import           Data.Tree.View (drawTree)
import           RON.Data.GTree (GTree)
import qualified RON.Data.GTree as GTree
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store (Handle, newHandle)
import           RON.Text.Serialize (serializeUuid)
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID
import           UnliftIO (MonadUnliftIO, liftIO)

import           Options

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
      RunNode _nodeOptions -> undefined
        -- runNode db nodeOptions
      RunUI _nodeOptions -> undefined
      --   forkLinked $ runNode db nodeOptions
      --   runUI' username db

loadTheTree :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m (Tree UUID)
loadTheTree db =
  runStore db do
    forest <- GTree.loadForest theTreeRef
    pure $ Node theTreeId $ map (fmap opId) forest

theTreeId :: UUID
theTreeId = $(UUID.liftName "theTree")

theTreeRef :: Ref GTree
theTreeRef = Ref theTreeId []
