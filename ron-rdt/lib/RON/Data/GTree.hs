{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Experimental!
module RON.Data.GTree (GTree, toTree) where

import           RON.Prelude

import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tree (Forest, Tree (Node))

import           RON.Data.Experimental (Replicated)
import qualified RON.Data.Experimental
import           RON.Types (Op (..), OpenFrame, UUID)
import qualified RON.UUID as UUID

-- | Grow-only tree
data GTree = GTree
  { object   :: UUID
  , children :: HashMap UUID (HashSet Op)
  }

instance Replicated GTree where
  replicatedTypeId = $(UUID.liftName "log")
  stateFromFrame = gtreeFromFrame

gtreeFromFrame :: UUID -> OpenFrame -> GTree
gtreeFromFrame object ops =
  GTree
    { object
    , children =
        HashMap.fromListWith
          (<>)
          [ (refId, HashSet.singleton op)
          | op@Op{opId, refId} <- ops, opId /= object
          ]
    }

toTree :: GTree -> Forest Op
toTree GTree{object, children} = go object where
  go i =
    [ Node op $ go opId
    | op@Op{opId} <-
        toList $ HashMap.findWithDefault mempty i children
    ]
