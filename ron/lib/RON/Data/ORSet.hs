{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet (ORSet) where

import qualified Data.Map.Strict as Map

import           RON.Internal.Prelude

import           RON.Data.Internal (Reducible (..), mkStateChunk)
import           RON.Types (Op' (..), UUID)

data SetItem = SetItem{itemIsAlive :: Bool, itemOriginalOp :: Op'}
    deriving (Eq, Show)

instance Semigroup SetItem where
    (<>) = minOn itemIsAlive

itemFromOp :: Op' -> (UUID, SetItem)
itemFromOp op@Op'{opEvent, opRef, opPayload} = (itemId, item) where
    itemIsAlive = not $ null opPayload
    itemId = if itemIsAlive then opEvent else opRef
    item = SetItem{itemIsAlive, itemOriginalOp = op}

newtype ORSet = ORSet (Map UUID SetItem)
    deriving (Eq, Show)

instance Semigroup ORSet where
    ORSet set1 <> ORSet set2 = ORSet $ Map.unionWith (<>) set1 set2

instance Monoid ORSet where
    mempty = ORSet mempty

instance Reducible ORSet where
    type OpType ORSet = "set"

    stateFromChunk = ORSet . Map.fromListWith (<>) . map itemFromOp

    stateToChunk (ORSet set) =
        mkStateChunk . sortOn opEvent . map itemOriginalOp $ Map.elems set
