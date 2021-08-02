{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Experimental!
module RON.Data.GTree (GTree, insert, loadForest) where

import           RON.Prelude

import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tree (Forest, Tree (Node))

import           RON.Error (MonadE)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Store (MonadStore, appendPatch, loadSubObjectLog)
import           RON.Types (Op (..), OpenFrame, UUID)
import           RON.Types.Experimental (Patch (..), Ref (..))

-- | Grow-only tree
data GTree

type GTreeState = HashMap UUID (HashSet Op)

readFrame :: UUID -> OpenFrame -> GTreeState
readFrame object ops =
  HashMap.fromListWith
    (<>)
    [(refId, HashSet.singleton op) | op@Op{opId, refId} <- ops, opId /= object]

toForest :: UUID -> GTreeState -> Forest Op
toForest object children = go object where
  go i =
    [ Node op $ go opId
    | op@Op{opId} <- toList $ HashMap.lookupDefault mempty i children
    ]

loadForest :: (MonadE m, MonadStore m) => Ref GTree -> m (Forest Op)
loadForest ref@(Ref oid _) =
  toForest oid . readFrame oid <$> loadSubObjectLog ref mempty

insert :: (MonadStore m, ReplicaClock m) => Ref GTree -> UUID -> m ()
insert (Ref object prefix) parent = do
  advanceToUuid object
  opId <- getEventUuid
  appendPatch
    Patch{object, log = Op{opId, refId = parent, payload = prefix} :| []}
