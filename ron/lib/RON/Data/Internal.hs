{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Internal where

import           RON.Internal.Prelude

import           GHC.TypeLits (KnownSymbol, Symbol)

import           RON.Types (Chunk, Op' (..), UUID)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> NonEmpty Chunk -> [Chunk]

-- | Unapplied patches and ops
type Unapplied = ([RChunk'], [Op'])

-- TODO(2018-08-24, cblp) Semilattice a
class (Eq a, Semigroup a, KnownSymbol (OpType a)) => Reducible a where

    type OpType a :: Symbol

    stateFromChunk :: [Op'] -> a

    -- | Result is a state chunk
    stateToChunk :: a -> ({- version -} UUID, [Op'])

    applyPatches :: a -> Unapplied -> (a, Unapplied)
    default applyPatches :: Monoid a => a -> Unapplied -> (a, Unapplied)
    applyPatches a (patches, ops) =
        ( a <> foldMap (patchValue . patchFromChunk) patches
            <> foldMap (patchValue . patchFromRawOp) ops
        , mempty
        )

    reduceUnappliedPatches :: Unapplied -> Unapplied
    reduceUnappliedPatches (patches, ops) =
        ( maybeToList .
            fmap (patchToChunk @a . sconcat) .
            nonEmpty $
            map patchFromChunk patches <> map patchFromRawOp ops
        , []
        )

data RChunk' = RChunk'
    { rchunk'Version :: UUID
    , rchunk'Ref     :: UUID
    , rchunk'Body    :: [Op']
    }
    deriving (Show)

mkChunkVersion :: [Op'] -> UUID
mkChunkVersion = maximumDef zero . map opEvent

mkRChunk' :: UUID -> [Op'] -> RChunk'
mkRChunk' ref rchunk'Body = RChunk'
    { rchunk'Version = mkChunkVersion rchunk'Body
    , rchunk'Ref = ref
    , rchunk'Body
    }

mkStateChunk :: [Op'] -> (UUID, [Op'])
mkStateChunk ops = (mkChunkVersion ops, ops)

data Patch a = Patch{patchRef :: UUID, patchValue :: a}

instance Semigroup a => Semigroup (Patch a) where
    Patch ref1 a1 <> Patch ref2 a2 = Patch (min ref1 ref2) (a1 <> a2)

patchFromRawOp :: Reducible a => Op' -> Patch a
patchFromRawOp op@Op'{opEvent} = Patch
    { patchRef = opEvent
    , patchValue = stateFromChunk [op]
    }

patchFromChunk :: Reducible a => RChunk' -> Patch a
patchFromChunk RChunk'{..} =
    Patch{patchRef = rchunk'Ref, patchValue = stateFromChunk rchunk'Body}

patchToChunk :: Reducible a => Patch a -> RChunk'
patchToChunk Patch{..} = RChunk'{..} where
    rchunk'Ref = patchRef
    (rchunk'Version, rchunk'Body) = stateToChunk patchValue
