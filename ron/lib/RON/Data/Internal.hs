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
                            StateChunk (..), UUID)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> NonEmpty Chunk -> [Chunk]

-- | Unapplied patches and ops
type Unapplied = ([RChunk'], [Op'])

-- TODO(2018-08-24, cblp) Semilattice a
class (Eq a, Semigroup a, KnownSymbol (OpType a)) => Reducible a where

    type OpType a :: Symbol

    stateFromChunk :: [Op'] -> a

    stateToChunk :: a -> StateChunk

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

mkStateChunk :: [Op'] -> StateChunk
mkStateChunk ops = StateChunk (mkChunkVersion ops) ops

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
    StateChunk rchunk'Version rchunk'Body = stateToChunk patchValue

class Replicated a where
    newRon :: Clock clock => a -> WriterT Frame' clock [Atom]
    default newRon
        :: (Clock clock, ReplicatedAsObject a)
        => a -> WriterT Frame' clock [Atom]
    newRon a = do
        Object (_, oid) frame <- lift $ newObject a
        tell frame
        pure [AUuid oid]

    fromRon :: [Atom] -> Frame' -> Either String a
    default fromRon
        :: ReplicatedAsObject a => [Atom] -> Frame' -> Either String a
    fromRon = objectFromPayload (objectOpType @a) getObject

instance Replicated Int64 where
    newRon int = pure [AInteger int]
    fromRon atoms _ = case atoms of
        [AInteger int] -> pure int
        _ -> Left "Int64: bad payload"

instance Replicated Text where
    newRon t = pure [AString t]
    fromRon atoms _ = case atoms of
        [AString t] -> pure t
        _ -> Left "String: bad payload"

instance Replicated Char where
    newRon c = pure [AString $ Text.singleton c]
    fromRon atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "Char: bad payload"

class ReplicatedAsObject a where
    objectOpType :: UUID
    newObject :: Clock clock => a -> clock (Object a)
    getObject :: Object a -> Either String a

objectFromPayload
    :: UUID
    -> (Object a -> Either String a)
    -> [Atom]
    -> Frame'
    -> Either String a
objectFromPayload typ handler atoms frame = case atoms of
    [AUuid oid] -> handler $ Object (typ, oid) frame
    _ -> Left "bad payload"

collectFrame
    :: forall a m
    . (ReplicatedAsObject a, Functor m) => WriterT Frame' m UUID -> m (Object a)
collectFrame =
    fmap (\(oid, frame) -> Object (objectOpType @a, oid) frame) . runWriterT

getObjectStateChunk :: Object a -> Either String StateChunk
getObjectStateChunk (Object (typ, oid) frame) =
    maybe (Left "no such object in chunk") Right $ Map.lookup (typ, oid) frame
