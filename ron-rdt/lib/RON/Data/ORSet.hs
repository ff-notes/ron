{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Observed-Remove Set (OR-Set)
module RON.Data.ORSet
    ( ORSet (..)
    , ORSetRaw
    , addNewRef
    , addRef
    , addValue
    , removeRef
    , removeValue
    ) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (Atom, Object (Object, frame, id),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk, stateBody, stateType, stateVersion),
                            UUID)
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

-- | Untyped OR-Set.
-- Implementation:
-- a map from the last change (creation or deletion) to the original op.
newtype ORSetRaw = ORSetRaw (Map UUID Op)
    deriving (Eq, Show)

opKey :: Op -> UUID
opKey Op{opId, refId} = case refId of
    Zero -> opId   -- alive
    _    -> refId  -- tombstone

observedRemove :: Op -> Op -> Op
observedRemove = maxOn refId

instance Semigroup ORSetRaw where
    ORSetRaw set1 <> ORSetRaw set2 =
        ORSetRaw $ Map.unionWith observedRemove set1 set2

instance Monoid ORSetRaw where
    mempty = ORSetRaw mempty

instance Reducible ORSetRaw where
    reducibleOpType = setType

    stateFromChunk ops =
        ORSetRaw $ Map.fromListWith observedRemove [(opKey op, op) | op <- ops]

    stateToChunk (ORSetRaw set) =
        mkStateChunk setType . sortOn opId $ Map.elems set

-- | Name-UUID to use as OR-Set type marker.
setType :: UUID
setType = $(UUID.liftName "set")

-- | Type-directing wrapper for typed OR-Set
newtype ORSet a = ORSet [a]

instance Replicated a => Replicated (ORSet a) where
    encoding = objectEncoding

instance Replicated a => ReplicatedAsObject (ORSet a) where
    objectOpType = setType

    newObject (ORSet items) = collectFrame $ do
        ops <- for items $ \item -> do
            event <- lift getEventUuid
            payload <- newRon item
            pure $ Op event Zero payload
        oid <- lift getEventUuid
        let stateVersion = maximumDef oid $ map opId ops
        tell $
            Map.singleton oid $
            StateChunk{stateType = setType, stateVersion, stateBody = ops}
        pure oid

    -- getObject
    --     :: forall item m orset itemRep
    --     . (Coercible (orset item) [item], MonadE m, ReplicatedAsPayload itemRep)
    --     => (itemRep -> m item) -> Object (orset item) -> m (orset item)
    getObject obj@Object{frame} = do
        StateChunk{stateBody} <- getObjectStateChunk obj
        mItems <- for stateBody $ \Op{refId, payload} -> case refId of
            Zero -> do
                item <- fromRon payload frame
                pure $ Just item
            _    -> pure Nothing
        pure . ORSet $ catMaybes mItems

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
commonAdd :: (ReplicatedAsPayload b, ReplicaClock m, MonadE m)
    => b -> StateT (Object a) m ()
commonAdd item = do
    obj@Object{id, frame} <- get
    StateChunk{..} <- getObjectStateChunk obj
    event <- getEventUuid
    let payload = toPayload item
    let newOp = Op event Zero payload
    let chunk' = stateBody ++ [newOp]
    let state' = StateChunk
            {stateType = setType, stateVersion = event, stateBody = chunk'}
    put obj{frame = Map.insert id state' frame}

-- | Add atomic value to the OR-Set
addValue
    :: (ReplicatedAsPayload a, ReplicaClock m, MonadE m)
    => a -> StateT (Object (ORSet a)) m ()
addValue = commonAdd

-- | Add a reference to the object to the OR-Set
addRef
    :: (ReplicaClock m, MonadE m) => Object a -> StateT (Object (ORSet a)) m ()
addRef Object{id = itemId, frame = itemFrame} = do
    modify' $ \Object{..} -> Object{frame = frame <> itemFrame, ..}
    commonAdd itemId

-- | Encode an object and add a reference to it to the OR-Set
addNewRef
    :: forall a m
    . (ReplicatedAsObject a, ReplicaClock m, MonadE m)
    => a -> StateT (Object (ORSet a)) m (Object a)
addNewRef item = do
    itemObj@(Object _ itemFrame) <- lift $ newObject item
    modify' $ \Object{..} -> Object{frame = frame <> itemFrame, ..}
    addRef itemObj
    pure itemObj

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove
    :: (MonadE m, ReplicaClock m)
    => ([Atom] -> Bool) -> StateT (Object (orset a)) m ()
commonRemove isTarget = do
    obj@Object{id, frame} <- get
    StateChunk{..} <- getObjectStateChunk obj
    let state0@(ORSetRaw opMap) = stateFromChunk stateBody
    let targetEvents =
            [ opId
            | Op{opId, refId, payload} <- toList opMap
            , refId == Zero  -- is alive
            , isTarget payload
            ]
    case targetEvents of
        [] -> pure ()
        _  -> do
            tombstone <- getEventUuid
            let patch =
                    [ Op{opId = tombstone, refId, payload = []}
                    | refId <- targetEvents
                    ]
            let chunk' = state0 <> stateFromChunk patch
            let state' = stateToChunk chunk'
            put obj{frame = Map.insert id state' frame}

-- | Remove an atomic value from the OR-Set
removeValue
    :: (ReplicatedAsPayload a, MonadE m, ReplicaClock m)
    => a -> StateT (Object (ORSet a)) m ()
removeValue = commonRemove . eqPayload

-- | Remove an object reference from the OR-Set
removeRef
    :: (MonadE m, ReplicaClock m) => Object a -> StateT (Object (ORSet a)) m ()
removeRef = commonRemove . eqRef
