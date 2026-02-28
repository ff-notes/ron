{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Database (loadTheTree, theTreeRef) where

import Control.Monad.Logger (MonadLogger)
import Data.Tree (Tree (Node))
import RON.Data.GTree (GTree)
import RON.Data.GTree qualified as GTree
import RON.Store.Sqlite (runStore)
import RON.Store.Sqlite qualified as Store (Handle)
import RON.Types (Op (..), UUID)
import RON.Types.Experimental (Ref (..))
import RON.UUID qualified as UUID
import UnliftIO (MonadUnliftIO)

loadTheTree :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m (Tree UUID)
loadTheTree db =
    runStore db do
        forest <- GTree.loadForest theTreeRef
        pure $ Node theTreeId $ map (fmap (.opId)) forest

theTreeId :: UUID
theTreeId = $(UUID.liftName "theTree")

theTreeRef :: Ref GTree
theTreeRef = Ref theTreeId
