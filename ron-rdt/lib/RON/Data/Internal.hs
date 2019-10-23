{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Data.Internal (
    ReducedChunk (..),
    Reducer (..),
    Reducible (..),
    Rep,
    Replicated (..),
    Unapplied,
    WireReducer,
    advanceToObject,
    eqPayload,
    eqRef,
    getObjectRep,
    reduceState,
    -- reduceObjectStates,
    stateFromWireChunk,
    stateToWireChunk,
    tryFromRon,
    tryOptionFromRon,
    wireStateChunk,
    pattern Some,
    --
    -- rconcat,
    ) where

import           RON.Prelude

import qualified Data.Text as Text

import           RON.Error (Error (Error), MonadE, correct, errorContext)
import           RON.Event (ReplicaClock, advanceToUuid)
import           RON.Semilattice (BoundedSemilattice)
import           RON.Types (Atom (AInteger, AString, AUuid),
                            ObjectRef (ObjectRef),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk), UUID (UUID), WireChunk,
                            WireStateChunk (WireStateChunk, stateBody, stateType))

-- | Reduce all chunks of specific type and object in the frame
type WireReducer = UUID -> NonEmpty WireChunk -> [WireChunk]

data Reducer = Reducer
    { wireReducer  :: WireReducer
    , stateReducer :: WireStateChunk -> WireStateChunk -> WireStateChunk
    }

-- | Unapplied patches and raw ops
type Unapplied = ([ReducedChunk], [Op])

-- | Untyped-reducible types.
-- Untyped means if this type is a container then the types of data contained in
-- it is not considered.
class (Eq a, BoundedSemilattice a) => Reducible a where

    -- | UUID of the type
    reducibleOpType :: UUID

    -- | Load a state from a state chunk.
    stateFromChunk :: [Op] -> a

    -- | Store a state to a state chunk
    stateToChunk :: a -> [Op]

    -- | Merge a state with patches and raw ops
    applyPatches :: a -> Unapplied -> (a, Unapplied)
    applyPatches a (patches, ops) =
        ( a <> foldMap (patchValue . patchFromChunk) patches
            <> foldMap (patchValue . patchFromRawOp) ops
        , mempty
        )

    -- | Merge patches and raw ops into bigger patches or throw obsolete ops
    reduceUnappliedPatches :: Unapplied -> Unapplied
    reduceUnappliedPatches (patches, ops) =
        ( maybeToList .
            fmap (patchToChunk @a . sconcat) .
            nonEmpty $
            map patchFromChunk patches <> map patchFromRawOp ops
        , []
        )

stateToWireChunk :: forall rep . Reducible rep => rep -> WireStateChunk
stateToWireChunk rep = wireStateChunk @rep $ StateChunk $ stateToChunk @rep rep

stateFromWireChunk
    :: forall a m . (MonadE m, Reducible a) => WireStateChunk -> m a
stateFromWireChunk WireStateChunk{stateType, stateBody} = do
    unless (stateType == reducibleOpType @a) $
        throwError $
        Error "bad type"
            [ Error ("expected " <> show (reducibleOpType @a)) []
            , Error ("got " <> show stateType) []
            ]
    pure $ stateFromChunk stateBody

data ReducedChunk = ReducedChunk
    { rcRef     :: UUID
    , rcBody    :: [Op]
    }
    deriving (Show)

data Patch a = Patch{patchRef :: UUID, patchValue :: a}

instance Semigroup a => Semigroup (Patch a) where
    Patch ref1 a1 <> Patch ref2 a2 = Patch (min ref1 ref2) (a1 <> a2)

patchFromRawOp :: Reducible a => Op -> Patch a
patchFromRawOp op@Op{opId} = Patch
    { patchRef = opId
    , patchValue = stateFromChunk [op]
    }

patchFromChunk :: Reducible a => ReducedChunk -> Patch a
patchFromChunk ReducedChunk{rcRef, rcBody} =
    Patch{patchRef = rcRef, patchValue = stateFromChunk rcBody}

patchToChunk :: Reducible a => Patch a -> ReducedChunk
patchToChunk Patch{patchRef, patchValue} =
    ReducedChunk{rcRef = patchRef, rcBody = stateBody}
  where
    WireStateChunk{stateBody} = stateToWireChunk patchValue

class Replicated a where

    -- | Encode data
    toPayload :: a -> Payload

    -- | Decode data
    fromPayload :: MonadE m => Payload -> m a

instance Replicated Int64 where
    toPayload int = [AInteger int]
    fromPayload atoms = errorContext "Integer" $ case atoms of
        [AInteger int] -> pure int
        _              -> throwError "Expected Integer"

instance Replicated UUID where
    toPayload u = [AUuid u]
    fromPayload atoms = errorContext "UUID" $ case atoms of
        [AUuid u] -> pure u
        _         -> throwError "Expected UUID"

instance Replicated Text where
    toPayload t = [AString t]
    fromPayload atoms = errorContext "String" $ case atoms of
        [AString t] -> pure t
        _           -> throwError "Expected String"

instance Replicated Char where
    toPayload c = [AString $ Text.singleton c]
    fromPayload atoms = errorContext "Char" $ case atoms of
        [AString (Text.uncons -> Just (c, ""))] -> pure c
        _ -> throwError "Expected one-character string"

-- | Raw RON representation of a view type.
type family Rep a
-- TODO? class Reducible (Rep a) => ReplicatedAsObject a where type Rep a

eqRef :: ObjectRef a -> Payload -> Bool
eqRef (ObjectRef uuid) atoms = case atoms of
    [AUuid ref] -> uuid == ref
    _           -> False

eqPayload :: Replicated a => a -> Payload -> Bool
eqPayload a atoms = toPayload a == atoms

pattern ATrue :: Atom
pattern ATrue = AUuid (UUID 0xe36e69000000000 0)  -- true

pattern AFalse :: Atom
pattern AFalse = AUuid (UUID 0xaa5c37a40000000 0)  -- false

instance Replicated Bool where
    toPayload b
        | b         = [ATrue]
        | otherwise = [AFalse]

    fromPayload = errorContext "Bool" . \case
        [ATrue]  -> pure True
        [AFalse] -> pure False
        _        -> throwError "Expected single UUID `true` or `false`"

advanceToObject ::
    (MonadE m, MonadState (StateChunk a) m, ReplicaClock m) =>
    ObjectRef a -> m ()
advanceToObject (ObjectRef uuid) = do
    StateChunk chunk <- get
    advanceToUuid $
        maximumDef
            uuid
            [ max opId $ maximumDef refId $ mapMaybe atomAsUuid payload
            | Op{opId, refId, payload} <- chunk
            ]
  where
    -- | TODO(2019-07-26, cblp) Use lens
    atomAsUuid = \case
        AUuid u -> Just u
        _       -> Nothing

reduceState ::
    forall rep.
    Reducible rep => StateChunk rep -> StateChunk rep -> StateChunk rep
reduceState (StateChunk s1) (StateChunk s2) =
    StateChunk $ stateToChunk @rep $ stateFromChunk s1 <> stateFromChunk s2

-- TODO requires MonadStorage
-- reduceObjectStates ::
--     forall a m.
--     (MonadE m, Reducible (Rep a)) => NonEmpty (ObjectRef a) -> m (ObjectRef a)
-- reduceObjectStates objs = do
--     c :| cs <- for objs getObjectStateChunk
--     let chunk = foldl' (reduceState @(Rep a)) c cs
--         oid = minimum [i | ObjectRef i <- toList objs]
--     modify' $ Map.insert oid $ wireStateChunk chunk
--     pure $ ObjectRef oid

-- TODO requires MonadStorage
-- rconcat ::
--     forall a m .
--     (MonadE m, Reducible (Rep a), Replicated a) => NonEmpty UUID -> m a
-- rconcat uuids =
--     errorContext "rconcat" $ do
--         ObjectRef ref <- reduceObjectStates @a $ ObjectRef <$> uuids
--         fromPayload [AUuid ref]

tryFromRon :: (MonadE m, Replicated a) => Payload -> m (Maybe a)
tryFromRon = correct Nothing . fmap Just . fromPayload

tryOptionFromRon :: (MonadE m, Replicated a) => Payload -> m (Maybe a)
tryOptionFromRon payload = case payload of
    Some : payload' ->
        correct Nothing $
            (Just <$> fromPayload payload)
            `catchError` \e1 ->
                (Just <$> fromPayload payload')
                `catchError` \e2 ->
                    throwError $ Error "tryOptionFromRon" [e1, e2]
    _ -> tryFromRon payload

-- | TODO(2019-08-06, cblp) Remove a year after release
-- (the release is planned on 2019-08; removal is planned on 2020-08)
pattern Some :: Atom
pattern Some = AUuid (UUID 0xdf3c69000000000 0)
{-# DEPRECATED Some "Will be removed soon" #-}

getObjectRep ::
    (MonadE m, MonadState (StateChunk a) m, Reducible (Rep a)) => m (Rep a)
getObjectRep = do
    StateChunk chunk <- get
    pure $ stateFromChunk chunk

wireStateChunk :: forall rep . Reducible rep => StateChunk rep -> WireStateChunk
wireStateChunk (StateChunk stateBody) =
    WireStateChunk{stateType = reducibleOpType @rep, stateBody}

instance Replicated (ObjectRef a) where
    toPayload (ObjectRef uuid) = toPayload uuid
    fromPayload = fmap ObjectRef . fromPayload
