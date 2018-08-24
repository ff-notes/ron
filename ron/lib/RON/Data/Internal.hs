{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Internal where

import           RON.Internal.Prelude

import           GHC.TypeLits (Symbol)
import           RON.Types (Chunk, ROp, UUID)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> [Chunk] -> [Chunk]

-- TODO(2018-08-24, cblp) Semilattice
class Semigroup a => Reducible a where
    {-# MINIMAL initial, toStateChunk, (applyOp | applyPatch) #-}

    type OpType a :: Symbol

    -- TODO(2018-08-24, cblp) Monoid.mempty?
    initial :: a

    fromStateChunk :: [ROp] -> Either String a
    fromStateChunk ops = applyPatch ops initial

    toStateChunk :: a -> [ROp]

    applyOp :: ROp -> a -> Either String a
    applyOp op = applyPatch [op]

    applyPatch :: [ROp] -> a -> Either String a
    applyPatch = \case
        []     -> pure
        op:ops -> applyOp op >=> applyPatch ops
