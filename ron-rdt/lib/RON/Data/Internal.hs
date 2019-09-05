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
    Encoding (..),
    ReducedChunk (..),
    Reducer (..),
    Reducible (..),
    Replicated (..),
    ReplicatedAsObject (..),
    ReplicatedAsPayload (..),
    Unapplied,
    WireReducer,
    advanceToObject,
    eqPayload,
    eqRef,
    evalObjectState,
    evalObjectState_,
    getObjectState,
    getObjectStateChunk,
    modifyObjectStateChunk,
    modifyObjectStateChunk_,
    newObjectFrame,
    reduceState,
    reduceObjectStates,
    stateFromWireChunk,
    stateToWireChunk,
    tryFromRon,
    tryOptionFromRon,
    wireStateChunk,
    pattern Some,
    --
    objectEncoding,
    rconcat,
    payloadEncoding,
    --
    fromRon,
    newRon,
    --
    ObjectStateT,
    MonadObjectState,
) where

import           RON.Prelude

import           Data.List (minimum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Error (Error (Error), MonadE, correct, errorContext,
                            liftMaybe)
import           RON.Event (ReplicaClock, advanceToUuid)
import           RON.Semilattice (BoundedSemilattice)
import           RON.Types (Atom (AInteger, AString, AUuid),
                            ObjectFrame (ObjectFrame, frame, uuid),
                            ObjectRef (ObjectRef),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk), StateFrame, UUID (UUID),
                            WireChunk,
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

-- | Base class for typed encoding
class Replicated a where
    -- | Instances SHOULD implement 'encoding' either as 'objectEncoding' or as
    -- 'payloadEncoding'
    encoding :: Encoding a

data Encoding a = Encoding
    { encodingNewRon
        :: forall m
        . (ReplicaClock m, MonadState StateFrame m) => a -> m Payload
    , encodingFromRon
        :: forall m . (MonadE m, MonadState StateFrame m) => Payload -> m a
    }

-- | Encode typed data to a payload with possible addition objects
newRon
    :: (Replicated a, ReplicaClock m, MonadState StateFrame m) => a -> m Payload
newRon = encodingNewRon encoding

-- | Decode typed data from a payload.
-- The implementation may use other objects in the frame to resolve references.
fromRon :: (MonadE m, Replicated a, MonadState StateFrame m) => Payload -> m a
fromRon = encodingFromRon encoding

-- | Standard implementation of 'Replicated' for 'ReplicatedAsObject' types.
objectEncoding :: ReplicatedAsObject a => Encoding a
objectEncoding = Encoding
    { encodingNewRon = \a -> do
        ObjectRef uuid <- newObject a
        pure [AUuid uuid]
    , encodingFromRon = objectFromRon $ runReaderT readObject
    }

-- | Standard implementation of 'Replicated' for 'ReplicatedAsPayload' types.
payloadEncoding :: ReplicatedAsPayload a => Encoding a
payloadEncoding = Encoding
    { encodingNewRon  = pure . toPayload
    , encodingFromRon = fromPayload
    }

-- | Instances of this class are encoded as payload only.
--
-- Law: @'encoding' == 'payloadEncoding'@
class Replicated a => ReplicatedAsPayload a where

    -- | Encode data
    toPayload :: a -> Payload

    -- | Decode data
    fromPayload :: MonadE m => Payload -> m a

instance Replicated Int64 where encoding = payloadEncoding

instance ReplicatedAsPayload Int64 where
    toPayload int = [AInteger int]
    fromPayload atoms = errorContext "Integer" $ case atoms of
        [AInteger int] -> pure int
        _              -> throwError "Expected Integer"

instance Replicated UUID where encoding = payloadEncoding

instance ReplicatedAsPayload UUID where
    toPayload u = [AUuid u]
    fromPayload atoms = errorContext "UUID" $ case atoms of
        [AUuid u] -> pure u
        _         -> throwError "Expected UUID"

instance Replicated Text where encoding = payloadEncoding

instance ReplicatedAsPayload Text where
    toPayload t = [AString t]
    fromPayload atoms = errorContext "String" $ case atoms of
        [AString t] -> pure t
        _           -> throwError "Expected String"

instance Replicated Char where encoding = payloadEncoding

instance ReplicatedAsPayload Char where
    toPayload c = [AString $ Text.singleton c]
    fromPayload atoms = errorContext "Char" $ case atoms of
        [AString (Text.uncons -> Just (c, ""))] -> pure c
        _ -> throwError "Expected one-character string"

-- | Instances of this class are encoded as objects.
-- An enclosing object's payload will be filled with this object's id.
--
-- Law: @'encoding' == 'objectEncoding'@
class (Reducible (Rep a), Replicated a) => ReplicatedAsObject a where

    -- | Untyped RON-RDT representation
    type Rep a

    -- | Encode data. Write frame and return id.
    newObject
        :: (ReplicaClock m, MonadState StateFrame m) => a -> m (ObjectRef a)

    -- | Decode data
    readObject :: (MonadE m, MonadObjectState a m) => m a

objectFromRon :: MonadE m => (ObjectRef a -> m a) -> Payload -> m a
objectFromRon handler atoms =
    errorContext "objectFromRon" $
        case atoms of
            [AUuid uuid] -> handler $ ObjectRef uuid
            _            -> throwError "Expected object UUID"

-- | Create new 'ObjectFrame' from a value
newObjectFrame
    :: (ReplicatedAsObject a, ReplicaClock m) => a -> m (ObjectFrame a)
newObjectFrame a = do
    (ObjectRef uuid, frame) <- runStateT (newObject a) mempty
    pure $ ObjectFrame{uuid, frame}

getObjectStateChunk
    :: forall a m . (MonadE m, MonadObjectState a m) => m (StateChunk (Rep a))
getObjectStateChunk = do
    ObjectRef uuid <- ask
    frame <- get
    WireStateChunk{stateType, stateBody} <-
        liftMaybe "no such object in chunk" $ Map.lookup uuid frame
    unless (stateType == reducibleOpType @(Rep a)) $
        throwError $
        Error "bad type"
            [ Error ("expected " <> show (reducibleOpType @(Rep a))) []
            , Error ("got " <> show stateType) []
            ]
    pure $ StateChunk stateBody

modifyObjectStateChunk
    :: forall a b m
    . (MonadObjectState a m, ReplicaClock m, MonadE m)
    => (StateChunk (Rep a) -> m (b, StateChunk (Rep a))) -> m b
modifyObjectStateChunk f = do
    advanceToObject
    ObjectRef uuid <- ask
    chunk <- getObjectStateChunk
    (a, StateChunk chunk') <- f chunk
    modify' $
        Map.insert uuid $
        WireStateChunk{stateType = reducibleOpType @(Rep a), stateBody = chunk'}
    pure a

modifyObjectStateChunk_
    :: (MonadObjectState a m, ReplicaClock m, MonadE m)
    => (StateChunk (Rep a) -> m (StateChunk (Rep a))) -> m ()
modifyObjectStateChunk_ f = modifyObjectStateChunk $ \chunk -> do
    chunk' <- f chunk
    pure ((), chunk')

eqRef :: ObjectRef a -> Payload -> Bool
eqRef (ObjectRef uuid) atoms = case atoms of
    [AUuid ref] -> uuid == ref
    _           -> False

eqPayload :: ReplicatedAsPayload a => a -> Payload -> Bool
eqPayload a atoms = toPayload a == atoms

pattern ATrue :: Atom
pattern ATrue = AUuid (UUID 0xe36e69000000000 0)  -- true

pattern AFalse :: Atom
pattern AFalse = AUuid (UUID 0xaa5c37a40000000 0)  -- false

instance Replicated Bool where encoding = payloadEncoding

instance ReplicatedAsPayload Bool where
    toPayload b
        | b         = [ATrue]
        | otherwise = [AFalse]

    fromPayload = errorContext "Bool" . \case
        [ATrue]  -> pure True
        [AFalse] -> pure False
        _        -> throwError "Expected single UUID `true` or `false`"

type ObjectStateT b m a = ReaderT (ObjectRef b) (StateT StateFrame m) a

type MonadObjectState a m =
    (MonadReader (ObjectRef a) m, MonadState StateFrame m, Reducible (Rep a))

advanceToObject :: (MonadE m, MonadObjectState a m, ReplicaClock m) => m ()
advanceToObject = do
    ObjectRef uuid <- ask
    StateChunk chunk <- getObjectStateChunk
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

reduceState
    :: forall rep
    . Reducible rep => StateChunk rep -> StateChunk rep -> StateChunk rep
reduceState (StateChunk s1) (StateChunk s2) =
    StateChunk $ stateToChunk @rep $ stateFromChunk s1 <> stateFromChunk s2

reduceObjectStates
    :: forall a m
    . (MonadE m, MonadState StateFrame m, ReplicatedAsObject a)
    => NonEmpty (ObjectRef a) -> m (ObjectRef a)
reduceObjectStates (obj :| objs) = do
    c :| cs <- for (obj :| objs) $ runReaderT getObjectStateChunk
    let chunk = foldl' (reduceState @(Rep a)) c cs
        oid = minimum [i | ObjectRef i <- obj:objs]
    modify' $ Map.insert oid $ wireStateChunk chunk
    pure $ ObjectRef oid

rconcat
    :: forall a m
    . (MonadE m, MonadState StateFrame m, ReplicatedAsObject a)
    => NonEmpty UUID -> m a
rconcat uuids =
    errorContext "rconcat" $ do
        ObjectRef ref <- reduceObjectStates @a $ ObjectRef <$> uuids
        fromRon [AUuid ref]

tryFromRon
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => Payload -> m (Maybe a)
tryFromRon = correct Nothing . fmap Just . fromRon

tryOptionFromRon
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => Payload -> m (Maybe a)
tryOptionFromRon payload = case payload of
    Some : payload' ->
        correct Nothing $
            (Just <$> fromRon payload)
            `catchError` \e1 ->
                (Just <$> fromRon payload')
                `catchError` \e2 ->
                    throwError $ Error "tryOptionFromRon" [e1, e2]
    _ -> tryFromRon payload

-- | TODO(2019-08-06, cblp) Remove a year after release
-- (the release is planned on 2019-08; removal is planned on 2020-08)
pattern Some :: Atom
pattern Some = AUuid (UUID 0xdf3c69000000000 0)
{-# DEPRECATED Some "Will be removed soon" #-}

getObjectState :: (MonadE m, MonadObjectState a m) => m (Rep a)
getObjectState = do
    StateChunk chunk <- getObjectStateChunk
    pure $ stateFromChunk chunk

wireStateChunk :: forall rep . Reducible rep => StateChunk rep -> WireStateChunk
wireStateChunk (StateChunk stateBody) =
    WireStateChunk{stateType = reducibleOpType @rep, stateBody}

-- | Run ObjectFrame action
evalObjectState :: Monad m => ObjectFrame b -> ObjectStateT b m a -> m a
evalObjectState ObjectFrame{uuid, frame} action =
    evalStateT (runReaderT action $ ObjectRef uuid) frame

-- | Run ObjectFrame action, starting with an empty frame
evalObjectState_ :: Monad m => StateT StateFrame m a -> m a
evalObjectState_ action = evalStateT action mempty
