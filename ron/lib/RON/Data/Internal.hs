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

import           RON.Types (Chunk, Op' (..), UUID)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> NonEmpty Chunk -> [Chunk]

-- TODO(2018-08-24, cblp) Semilattice a
class (Semigroup a, KnownSymbol (OpType a)) => Reducible a where

    type OpType a :: Symbol

    fromRawOp :: Op' -> a

    fromChunk
        :: UUID  -- ^ ref event, 0 for state chunks
        -> [Op']
        -> a

    toChunks :: a -> Reduced

    sameState :: a -> a -> Bool
    default sameState :: Eq a => a -> a -> Bool
    sameState = (==)

    -- () -> [Patch] -> [Op] -> ([Patch], [Op])
    -- NonEmpty State -> [Patch] -> [Op] -> (State, [Patch], [Op])

data Reduced = Reduced
    { reducedStateVersion :: UUID
    , reducedStateBody    :: [Op']
    , reducedPatches      :: [RChunk']
    , reducedUnappliedOps :: [Op']
    }

data RChunk' = RChunk'
    { rchunk'Version :: UUID
    , rchunk'Ref     :: UUID
    , rchunk'Body    :: [Op']
    }

mkReducedState :: [Op'] -> Reduced
mkReducedState reducedStateBody = Reduced
    { reducedStateVersion = ropsEvent reducedStateBody
    , reducedStateBody
    , reducedPatches = []
    , reducedUnappliedOps = []
    }

mkReducedPatch :: UUID -> [Op'] -> Reduced
mkReducedPatch ref ops = Reduced
    { reducedStateVersion = zero
    , reducedStateBody = []
    , reducedPatches = [mkRChunk' ref ops]
    , reducedUnappliedOps = []
    }

ropsEvent :: [Op'] -> UUID
ropsEvent = maximumDef zero . map opEvent

mkRChunk' :: UUID -> [Op'] -> RChunk'
mkRChunk' ref rchunk'Body = RChunk'
    {rchunk'Version = ropsEvent rchunk'Body, rchunk'Ref = ref, rchunk'Body}
