{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet (ORSet) where

import qualified Data.Map.Strict as Map

import           RON.Internal.Prelude

import           RON.Data.Internal (Reducible (..), mkReducedPatch,
                                    mkReducedState)
import           RON.Types (Op' (..), UUID)
import           RON.UUID (pattern Zero)

data SetItem = SetItem{itemIsAlive :: Bool, itemOriginalOp :: Op'}
    deriving (Eq)

instance Semigroup SetItem where
    (<>) = minOn itemIsAlive

itemFromOp :: Op' -> (UUID, SetItem)
itemFromOp op@Op'{opEvent, opRef, opPayload} = (itemId, item) where
    itemIsAlive = not $ null opPayload
    itemId = if itemIsAlive then opEvent else opRef
    item = SetItem{itemIsAlive, itemOriginalOp = op}

data ORSet = ORSet{setRef :: Maybe UUID, setItems :: Map UUID SetItem}
    deriving (Eq)

instance Semigroup ORSet where
    ORSet ref1 items1 <> ORSet ref2 items2 =
        ORSet (min ref1 ref2) (Map.unionWith (<>) items1 items2)

instance Monoid ORSet where
    mempty = ORSet Nothing mempty

instance Reducible ORSet where
    type OpType ORSet = "set"

    fromRawOp op@Op'{opEvent} = ORSet
        { setRef = Just opEvent
        , setItems = uncurry Map.singleton $ itemFromOp op
        }

    fromChunk ref ops = ORSet
        { setRef = Just ref
        , setItems = Map.fromListWith (<>) $ map itemFromOp ops
        }

    toChunks ORSet{setRef, setItems} = case fromMaybe Zero setRef of
        Zero -> mkReducedState     ops
        ref  -> mkReducedPatch ref ops
      where
        ops = sortOn opEvent . map itemOriginalOp $ Map.elems setItems

    sameState = (==) `on` setItems
