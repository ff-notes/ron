{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Experimental!
module RON.Data.GTree (GTree, loadForest) where

import           RON.Prelude

import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tree (Forest, Tree (Node))

import           RON.Error (MonadE)
import           RON.Store (MonadStore, loadSubObjectLog)
import           RON.Types (Op (..), OpenFrame, UUID)
import           RON.Types.Experimental (Ref (..))

-- | Grow-only tree
newtype GTree = GTree (HashMap UUID (HashSet Op))

readFrame :: UUID -> OpenFrame -> GTree
readFrame object ops =
  GTree $
    HashMap.fromListWith
      (<>)
      [ (refId, HashSet.singleton op)
      | op@Op{opId, refId} <- ops, opId /= object
      ]

toForest :: UUID -> GTree -> Forest Op
toForest object (GTree children) = go object where
  go i =
    [ Node op $ go opId
    | op@Op{opId} <-
        toList $ HashMap.lookupDefault mempty i children
    ]

loadForest :: (MonadE m, MonadStore m) => Ref GTree -> m (Forest Op)
loadForest object@(Ref objectId _) = do
  ops <- loadSubObjectLog object mempty
  let state = readFrame objectId ops
  pure $ toForest objectId state
