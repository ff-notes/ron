{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Internal where

import           RON.Internal.Prelude

import           Control.Monad.Writer.Strict (WriterT, lift, runWriterT, tell)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Event (Clock)
import           RON.Types (Atom (..), Chunk, Frame', Object (..), Op' (..),
                            StateChunk, UUID)
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
    , ..
    }

mkStateChunk :: [Op'] -> (UUID, [Op'])
mkStateChunk ops = (mkChunkVersion ops, ops)

data Patch a = Patch{patchRef :: UUID, patchValue :: a}

instance Semigroup a => Semigroup (Patch a) where
    Patch ref1 a1 <> Patch ref2 a2 = Patch (min ref1 ref2) (a1 <> a2)

patchFromRawOp :: Reducible a => Op' -> Patch a
patchFromRawOp op@Op'{..} = Patch
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

class ReplicatedAsPayload a where
    newPayload :: Clock clock => a -> WriterT Frame' clock [Atom]
    default newPayload
        :: (Clock clock, ReplicatedAsObject a)
        => a -> WriterT Frame' clock [Atom]
    newPayload a = do
        Object oid frame <- lift $ newObject a
        tell frame
        pure [AUuid oid]

    fromPayload :: [Atom] -> Frame' -> Either String a
    default fromPayload
        :: ReplicatedAsObject a => [Atom] -> Frame' -> Either String a
    fromPayload = objectFromPayload getObject

instance ReplicatedAsPayload Int64 where
    newPayload int = pure [AInteger int]
    fromPayload atoms _ = case atoms of
        [AInteger int] -> pure int
        _ -> Left "Int64: bad payload"

instance ReplicatedAsPayload Text where
    newPayload t = pure [AString t]
    fromPayload atoms _ = case atoms of
        [AString t] -> pure t
        _ -> Left "String: bad payload"

instance ReplicatedAsPayload Char where
    newPayload c = pure [AString $ Text.singleton c]
    fromPayload atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "Char: bad payload"

class ReplicatedAsObject a where
    newObject :: Clock clock => a -> clock (Object Frame')
    getObject :: UUID -> Frame' -> Either String a

objectFromPayload
    :: (UUID -> Frame' -> Either String b)
    -> [Atom]
    -> Frame'
    -> Either String b
objectFromPayload handler atoms frame = case atoms of
    [AUuid oid] -> handler oid frame
    _ -> Left "bad payload"

collectFrame :: Functor m => WriterT frame m UUID -> m (Object frame)
collectFrame = fmap (uncurry Object) . runWriterT

getObjectStateChunk :: UUID -> UUID -> Frame' -> Either String StateChunk
getObjectStateChunk typ oid frame =
    maybe (Left "no such object in chunk") Right $ Map.lookup (typ, oid) frame
