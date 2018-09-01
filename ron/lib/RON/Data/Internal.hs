{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Internal where

import           RON.Internal.Prelude

import           GHC.TypeLits (KnownSymbol, Symbol)

import           RON.Types (Chunk, ROp (..), UUID)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> NonEmpty Chunk -> [Chunk]

-- TODO(2018-08-24, cblp) Semilattice a, Semilattice (Patch a)
class (Semigroup a, KnownSymbol (OpType a)) => Reducible a where

    type OpType a :: Symbol

    fromRawOp :: ROp -> a

    fromChunk
        :: UUID  -- ^ ref event, 0 for state chunks
        -> [ROp]
        -> a

    toChunks :: a -> Reduced

    sameState :: a -> a -> Bool
    default sameState :: Eq a => a -> a -> Bool
    sameState = (==)

data Reduced = Reduced
    { reducedStateVersion :: UUID
    , reducedStateBody    :: [ROp]
    , reducedPatches      :: [RChunk']
    , reducedUnappliedOps :: [ROp]
    }

data RChunk' = RChunk'
    { rchunk'Version :: UUID
    , rchunk'Ref     :: UUID
    , rchunk'Body    :: [ROp]
    }

mkReducedState :: [ROp] -> Reduced
mkReducedState reducedStateBody = Reduced
    { reducedStateVersion = ropsEvent reducedStateBody
    , reducedStateBody
    , reducedPatches = []
    , reducedUnappliedOps = []
    }

mkReducedPatch :: UUID -> [ROp] -> Reduced
mkReducedPatch ref ops = Reduced
    { reducedStateVersion = zero
    , reducedStateBody = []
    , reducedPatches = [mkRChunk' ref ops]
    , reducedUnappliedOps = []
    }

ropsEvent :: [ROp] -> UUID
ropsEvent = maximumDef zero . map ropEvent

mkRChunk' :: UUID -> [ROp] -> RChunk'
mkRChunk' ref rchunk'Body = RChunk'
    {rchunk'Version = ropsEvent rchunk'Body, rchunk'Ref = ref, rchunk'Body}
