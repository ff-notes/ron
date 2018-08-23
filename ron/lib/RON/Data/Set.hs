{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Set (Set) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (OpType, Reducible, stateFromChunk,
                                    stateToChunk)
import           RON.Types (ROp (ROp), UUID, ropEvent, ropLocation, ropPayload)

data SetItem = SetItem{itemIsAlive :: Bool, itemOriginalOp :: ROp}
    deriving (Eq)

instance Semigroup SetItem where
    (<>) = minOn itemIsAlive

itemFromOp :: ROp -> (UUID, SetItem)
itemFromOp op@ROp{ropEvent, ropLocation, ropPayload} = (itemId, item) where
    itemIsAlive = not $ null ropPayload
    itemId = if itemIsAlive then ropEvent else ropLocation
    item = SetItem{itemIsAlive, itemOriginalOp = op}

newtype Set = Set (Map UUID SetItem)
    deriving (Eq)

instance Semigroup Set where
    Set items1 <> Set items2 = Set $ Map.unionWith (<>) items1 items2

instance Monoid Set where
    mempty = Set mempty

instance Reducible Set where
    type OpType Set = "set"

    stateFromChunk = Set . Map.fromListWith (<>) . map itemFromOp

    stateToChunk (Set items) =
        sortOn ropEvent $ map itemOriginalOp $ Map.elems items
