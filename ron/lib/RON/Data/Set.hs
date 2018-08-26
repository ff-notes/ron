{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Set (Set) where

import           RON.Internal.Prelude hiding (Set)

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), mkReducedPatch,
                                    mkReducedState)
import           RON.Types (ROp (..), UUID)
import           RON.UUID (pattern Zero)

data SetItem = SetItem{itemIsAlive :: Bool, itemOriginalOp :: ROp}
    deriving (Eq)

instance Semigroup SetItem where
    (<>) = minOn itemIsAlive

itemFromOp :: ROp -> (UUID, SetItem)
itemFromOp op@ROp{ropEvent, ropLocation, ropPayload} = (itemId, item) where
    itemIsAlive = not $ null ropPayload
    itemId = if itemIsAlive then ropEvent else ropLocation
    item = SetItem{itemIsAlive, itemOriginalOp = op}

data Set = Set{setRef :: Maybe UUID, setItems :: Map UUID SetItem}
    deriving (Eq)

instance Semigroup Set where
    Set ref1 items1 <> Set ref2 items2 =
        Set (min ref1 ref2) (Map.unionWith (<>) items1 items2)

instance Monoid Set where
    mempty = Set Nothing mempty

instance Reducible Set where
    type OpType Set = "set"

    fromRawOp op@ROp{ropEvent} = Set
        { setRef = Just ropEvent
        , setItems = uncurry Map.singleton $ itemFromOp op
        }

    fromChunk ref ops = Set
        { setRef = Just ref
        , setItems = Map.fromListWith (<>) $ map itemFromOp ops
        }

    toChunks Set{setRef, setItems} = case fromMaybe Zero setRef of
        Zero -> mkReducedState     ops
        ref  -> mkReducedPatch ref ops
      where
        ops = sortOn ropEvent . map itemOriginalOp $ Map.elems setItems

    sameState = (==) `on` setItems
