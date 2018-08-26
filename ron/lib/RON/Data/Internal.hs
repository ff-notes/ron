{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Internal where

import           GHC.TypeLits (KnownSymbol, Symbol)
import           RON.Types (Chunk, ROp, UUID)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> [Chunk] -> [Chunk]

-- TODO(2018-08-24, cblp) Semilattice a, Semilattice (Patch a)
class (Monoid a, KnownSymbol (OpType a), Semigroup (Patch a)) => Reducible a
    where

    type OpType a :: Symbol

    type Patch a
    type Patch a = a

    stateFromChunk :: [ROp] -> a

    stateToChunk :: a -> [ROp]

    patchFromOp :: ROp -> Patch a
    default patchFromOp :: Patch a ~ a => ROp -> Patch a
    patchFromOp rop = stateFromChunk [rop]

    patchFromChunk :: [ROp] -> Patch a
    default patchFromChunk :: Patch a ~ a => [ROp] -> Patch a
    patchFromChunk = stateFromChunk

    patchToChunk :: Patch a -> [ROp]
    default patchToChunk :: Patch a ~ a => Patch a -> [ROp]
    patchToChunk = stateToChunk

    applyPatch :: a -> Patch a -> a
    default applyPatch :: Patch a ~ a => a -> Patch a -> a
    applyPatch = (<>)
