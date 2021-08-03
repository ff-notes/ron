module Database (loadTheTree, theTreeRef) where

import           Control.Monad.Logger (MonadLogger)
import           Data.Tree (Tree (Node))
import           RON.Data.GTree (GTree)
import qualified RON.Data.GTree as GTree
import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store (Handle)
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID
import           UnliftIO (MonadUnliftIO)

loadTheTree :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m (Tree UUID)
loadTheTree db =
  runStore db do
    forest <- GTree.loadForest theTreeRef
    pure $ Node theTreeId $ map (fmap opId) forest

theTreeId :: UUID
theTreeId = $(UUID.liftName "theTree")

theTreeRef :: Ref GTree
theTreeRef = Ref theTreeId []
