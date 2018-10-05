{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Data.Internal where

import           RON.Internal.Prelude

import           Control.Monad.Writer.Strict (WriterT, lift, runWriterT, tell)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Event (Clock)
import           RON.Types (Atom (..), Chunk, Object (..), Op (..),
                            StateChunk (..), StateFrame, UUID)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> NonEmpty Chunk -> [Chunk]

-- | Unapplied patches and ops
type Unapplied = ([RChunk'], [Op])

-- TODO(2018-08-24, cblp) Semilattice a
class (Eq a, Semigroup a) => Reducible a where
    reducibleOpType :: UUID

    stateFromChunk :: [Op] -> a

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
    , rchunk'Body    :: [Op]
    }
    deriving (Show)

mkChunkVersion :: [Op] -> UUID
mkChunkVersion = maximumDef zero . map opEvent

mkRChunk' :: UUID -> [Op] -> RChunk'
mkRChunk' ref rchunk'Body = RChunk'
    { rchunk'Version = mkChunkVersion rchunk'Body
    , rchunk'Ref = ref
    , ..
    }

mkStateChunk :: [Op] -> StateChunk
mkStateChunk ops = StateChunk (mkChunkVersion ops) ops

data Patch a = Patch{patchRef :: UUID, patchValue :: a}

instance Semigroup a => Semigroup (Patch a) where
    Patch ref1 a1 <> Patch ref2 a2 = Patch (min ref1 ref2) (a1 <> a2)

patchFromRawOp :: Reducible a => Op -> Patch a
patchFromRawOp op@Op{..} = Patch
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
    encoding :: Encoding a

data Encoding a = Encoding
    { encodingNewRon
        :: forall clock . Clock clock => a -> WriterT StateFrame clock [Atom]
    , encodingFromRon :: [Atom] -> StateFrame -> Either String a
    }

newRon :: (Replicated a, Clock clock) => a -> WriterT StateFrame clock [Atom]
newRon = encodingNewRon encoding

fromRon :: Replicated a => [Atom] -> StateFrame -> Either String a
fromRon = encodingFromRon encoding

objectEncoding :: ReplicatedAsObject a => Encoding a
objectEncoding = Encoding
    { encodingNewRon = \a -> do
        Object oid frame <- lift $ newObject a
        tell frame
        pure [AUuid oid]
    , encodingFromRon = objectFromRon getObject
    }

payloadEncoding :: ReplicatedAsPayload a => Encoding a
payloadEncoding = Encoding
    { encodingNewRon  = pure . toPayload
    , encodingFromRon = \atoms _ -> fromPayload atoms
    }

class ReplicatedAsPayload a where
    toPayload :: a -> [Atom]
    fromPayload :: [Atom] -> Either String a

instance Replicated Int64 where encoding = payloadEncoding

instance ReplicatedAsPayload Int64 where
    toPayload int = [AInteger int]
    fromPayload atoms = case atoms of
        [AInteger int] -> pure int
        _ -> Left "Int64: bad payload"

instance Replicated UUID where encoding = payloadEncoding

instance ReplicatedAsPayload UUID where
    toPayload u = [AUuid u]
    fromPayload atoms = case atoms of
        [AUuid u] -> pure u
        _ -> Left "UUID: bad payload"

instance Replicated Text where encoding = payloadEncoding

instance ReplicatedAsPayload Text where
    toPayload t = [AString t]
    fromPayload atoms = case atoms of
        [AString t] -> pure t
        _           -> Left "String: bad payload"

instance Replicated Char where encoding = payloadEncoding

instance ReplicatedAsPayload Char where
    toPayload c = [AString $ Text.singleton c]
    fromPayload atoms = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _            -> Left "too long string to encode a single character"
        _ -> Left "Char: bad payload"

class ReplicatedAsObject a where
    objectOpType :: UUID
    newObject :: Clock clock => a -> clock (Object a)
    getObject :: Object a -> Either String a

objectFromRon
    :: (Object a -> Either String a) -> [Atom] -> StateFrame -> Either String a
objectFromRon handler atoms frame = case atoms of
    [AUuid oid] -> handler $ Object oid frame
    _ -> Left "bad payload"

collectFrame :: Functor m => WriterT StateFrame m UUID -> m (Object a)
collectFrame = fmap (uncurry Object) . runWriterT

getObjectStateChunk
    :: forall a . ReplicatedAsObject a => Object a -> Either String StateChunk
getObjectStateChunk (Object oid frame) =
    maybe (Left "no such object in chunk") Right $
    Map.lookup (objectOpType @a, oid) frame

eqRef :: Object a -> [Atom] -> Bool
eqRef (Object oid _) atoms = case atoms of
    [AUuid ref] -> oid == ref
    _           -> False

eqPayload :: ReplicatedAsPayload a => a -> [Atom] -> Bool
eqPayload a atoms = toPayload a == atoms

instance Replicated a => Replicated (Maybe a) where
    encoding = Encoding
        { encodingNewRon = \case
            Just a  -> newRon a
            Nothing -> pure []
        , encodingFromRon = \atoms frame -> case atoms of
            [] -> pure Nothing
            _  -> Just <$> fromRon atoms frame
        }
