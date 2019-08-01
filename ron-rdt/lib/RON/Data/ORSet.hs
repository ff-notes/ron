{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Observed-Remove Set (OR-Set)
module RON.Data.ORSet (
    ORSet (..),
    ORSetItem (..),
    ORSetMap,
    ORSetRep,
    addRef,
    addValue,
    findAnyAlive,
    findAnyAlive',
    removeRef,
    removeValue,
    zoomItem,
    -- * struct_set
    assignField,
    newStruct,
    viewField,
    viewFieldLWW,
    viewFieldMax,
    viewFieldMin,
    viewFieldSet,
    zoomFieldObject,
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Error (MonadE, errorContext, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Types (Atom (AUuid), Object (Object),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk, stateBody, stateType),
                            StateFrame, UUID)
import           RON.Util (Instance (Instance))
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

-- | Untyped OR-Set.
-- Implementation:
-- a map from the last change (creation or deletion) to the original op.
newtype ORSetRep = ORSetRep (Map UUID Op)
    deriving (Eq, Show)

opKey :: Op -> UUID
opKey Op{opId, refId} = case refId of
    Zero -> opId   -- alive
    _    -> refId  -- tombstone

observedRemove :: Op -> Op -> Op
observedRemove = maxOn refId

instance Semigroup ORSetRep where
    ORSetRep set1 <> ORSetRep set2 =
        ORSetRep $ Map.unionWith observedRemove set1 set2

instance Monoid ORSetRep where
    mempty = ORSetRep mempty

-- | Laws:
-- 1. Idempotent because 'Map.unionWith' is idempotent.
-- 2. Commutative because 'observedRemove' is commutative.
instance Semilattice ORSetRep

instance Reducible ORSetRep where
    reducibleOpType = setType

    stateFromChunk ops =
        ORSetRep $ Map.fromListWith observedRemove [(opKey op, op) | op <- ops]

    stateToChunk (ORSetRep set) = mkStateChunk . sortOn opId $ Map.elems set

mkStateChunk :: [Op] -> StateChunk
mkStateChunk stateBody = StateChunk{stateType = setType, stateBody}

-- | Name-UUID to use as OR-Set type marker.
setType :: UUID
setType = $(UUID.liftName "set")

-- | Type-directing wrapper for typed OR-Set.
-- 'Eq' instance is purely technical, it doesn't use 'Ord', nor 'Hashable',
-- so its result may be confusing.
newtype ORSet a = ORSet [a]
    deriving (Eq, Show)
{- TODO(2019-07-24, cblp, #102) data family ORSet c a where
    newtype instance ORSet Ord      a = ORSetOrd  (Set     a)
    newtype instance ORSet Hashable a = ORSetHash (HashSet a)
-}

instance Replicated a => Replicated (ORSet a) where
    encoding = objectEncoding

instance Replicated a => ReplicatedBoundedSemilattice (ORSet a) where
    rconcat = objectRconcat

instance Replicated a => ReplicatedAsObject (ORSet a) where
    type Rep (ORSet a) = ORSetRep

    newObject (ORSet items) = do
        ops <- for items $ \item -> do
            event <- getEventUuid
            payload <- newRon item
            pure $ Op event Zero payload
        oid <- getEventUuid
        modify' $
            (<>) $ Map.singleton oid $
            StateChunk{stateType = setType, stateBody = ops}
        pure $ Object oid

    getObject = do
        StateChunk{stateBody} <- getObjectStateChunk
        mItems <- for stateBody $ \Op{refId, payload} -> case refId of
            Zero -> do
                item <- fromRon payload
                pure $ Just item
            _    -> pure Nothing
        pure . ORSet $ catMaybes mItems

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
commonAdd :: (MonadE m, MonadObjectState a m, ReplicaClock m) => Payload -> m ()
commonAdd payload =
    modifyObjectStateChunk_ $ \StateChunk{stateBody} -> do
        event <- getEventUuid
        let newOp = Op event Zero payload
        let chunk' = stateBody ++ [newOp]
        pure StateChunk{stateType = setType, stateBody = chunk'}

-- | Encode a value and add a it to the OR-Set
addValue
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState (ORSet a) m)
    => a -> m ()
addValue item = do
    payload <- newRon item
    commonAdd payload

addRef
    :: (ReplicaClock m, MonadE m, MonadObjectState (ORSet a) m)
    => Object a -> m ()
addRef (Object itemUuid) = commonAdd [AUuid itemUuid]

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m)
    => (Payload -> Bool) -> m ()
commonRemove isTarget =
    modifyObjectStateChunk_ $ \chunk@StateChunk{stateBody} -> do
        let state0@(ORSetRep opMap) = stateFromChunk stateBody
        let targetEvents =
                [ opId
                | Op{opId, refId, payload} <- toList opMap
                , refId == Zero  -- is alive
                , isTarget payload
                ]
        case targetEvents of
            [] -> pure chunk
            _  -> do
                tombstone <- getEventUuid
                let patch =
                        [ Op{opId = tombstone, refId, payload = []}
                        | refId <- targetEvents
                        ]
                let state' = state0 <> stateFromChunk patch
                pure $ stateToChunk state'

-- | Remove an atomic value from the OR-Set
removeValue
    ::  ( ReplicatedAsPayload a
        , MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m
        )
    => a -> m ()
removeValue = commonRemove . eqPayload

-- | Remove an object reference from the OR-Set
removeRef
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m)
    => Object a -> m ()
removeRef = commonRemove . eqRef

-- | Reference to an item inside an 'ORSet'.
newtype ORSetItem a = ORSetItem UUID
    deriving (Show)

-- | Go from modification of the whole set to the modification of an item
-- object.
zoomItem
    :: MonadE m
    => ORSetItem item -> ObjectStateT item m a -> ObjectStateT (ORSet item) m a
zoomItem (ORSetItem key) innerModifier = do
    StateChunk{stateBody} <- getObjectStateChunk
    let ORSetRep opMap = stateFromChunk stateBody
    itemValueRef <- case Map.lookup key opMap of
        Nothing ->
            -- TODO(2019-07-08, cblp) create empty object?
            throwErrorText "no such key in ORSet"
        Just Op{payload} -> case payload of
            [AUuid itemValueRef] -> pure itemValueRef
            _ -> throwErrorText "item payload is not an object ref"
    lift $ runReaderT innerModifier $ Object itemValueRef

-- | Find any alive item. If no alive item found, return 'Nothing'.
findAnyAlive
    :: (MonadE m, MonadObjectState (ORSet item) m) => m (Maybe (ORSetItem item))
findAnyAlive = do
    StateChunk{stateBody} <- getObjectStateChunk
    pure $ let
        ORSetRep opMap = stateFromChunk stateBody
        aliveItems = [op | op@Op{refId = Zero} <- toList opMap]
        in
        case listToMaybe aliveItems of
            Nothing       -> Nothing
            Just Op{opId} -> Just $ ORSetItem opId

-- | Find any alive item. If no alive item found, report an error.
findAnyAlive'
    :: (MonadE m, MonadObjectState (ORSet item) m) => m (ORSetItem item)
findAnyAlive' = do
    mx <- findAnyAlive
    case mx of
        Just x  -> pure x
        Nothing -> throwErrorText "empty set"

type ORSetMap k v = ORSet (k, v)

-- | Assign a value to a field
assignField
    :: (Replicated field, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID   -- ^ Field name
    -> field  -- ^ Value
    -> m ()
assignField field value =
    modifyObjectStateChunk_ $ \StateChunk{stateBody} -> do
        event <- getEventUuid
        valuePayload <- newRon value
        let addOp = Op
                { opId = event
                , refId = Zero
                , payload = AUuid field : valuePayload
                }
        let (observed, stateBody1) = partition (isAliveField field) stateBody
        removeOps <- for observed $ \op -> do
            tombstone <- getEventUuid  -- TODO(2019-07-10, cblp) sequential
            pure op{refId = tombstone, payload = [AUuid field]}
        let stateBody2 = sortOn opId $ addOp : removeOps ++ stateBody1
        pure StateChunk{stateBody = stateBody2, stateType = setType}

isAliveField :: UUID -> Op -> Bool
isAliveField field = \case
    Op{refId = Zero, payload = AUuid field' : _} -> field == field'
    _ -> False

filterAliveFieldPayloads
    :: UUID               -- ^ field
    -> [Op]               -- ^ state body
    -> [Payload]  -- ^ value payloads
filterAliveFieldPayloads field ops =
    [ valuePayload
    | Op{refId = Zero, payload = AUuid field' : valuePayload} <- ops
    , field' == field
    ]

filterAliveFieldIdsAndPayloads
    :: UUID               -- ^ field
    -> [Op]               -- ^ state body
    -> [(UUID, Payload)]  -- ^ op ids and value payloads
filterAliveFieldIdsAndPayloads field ops =
    [ (opId, valuePayload)
    | Op{opId, refId = Zero, payload = AUuid field' : valuePayload} <- ops
    , field' == field
    ]

-- | Decode field value, merge all versions, return 'Nothing' if no versions
viewField
    :: (MonadE m, MonadState StateFrame m, ReplicatedBoundedSemilattice a)
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ ORSet object chunk
    -> m (Maybe a)
viewField field StateChunk{stateBody} =
    case filterAliveFieldPayloads field stateBody of
        []   -> pure Nothing
        p:ps -> fmap Just . rconcat $ p :| ps

-- | Decode field value, keep last version only
viewFieldLWW
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => UUID         -- ^ Field name
    -> StateChunk   -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldLWW field StateChunk{stateBody} =
    errorContext "ORSet.viewFieldLWW" $
    traverse fromRon $
    fmap snd . maximumMayOn fst $
    filterAliveFieldIdsAndPayloads field stateBody

-- | Decode field value, keep max value only, only for Integer and Float
viewFieldMax
    :: (MonadE m, Ord a, ReplicatedAsPayload a)
    => UUID         -- ^ Field name
    -> StateChunk   -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldMax field StateChunk{stateBody} =
    errorContext "ORSet.viewFieldMax" $
    fmap maximumMay $
    traverse fromPayload $
    filterAliveFieldPayloads field stateBody

-- | Decode field value, keep min value only, only for Integer and Float
viewFieldMin
    :: (MonadE m, Ord a, ReplicatedAsPayload a)
    => UUID         -- ^ Field name
    -> StateChunk   -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldMin field StateChunk{stateBody} =
    errorContext "ORSet.viewFieldMin" $
    fmap minimumMay $
    traverse fromPayload $
    filterAliveFieldPayloads field stateBody

-- | Decode field value, keep all versions
viewFieldSet
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => UUID         -- ^ Field name
    -> StateChunk   -- ^ ORSet object chunk
    -> m [a]
viewFieldSet field StateChunk{stateBody} =
    errorContext "ORSet.viewFieldSet" $
    traverse fromRon $
    filterAliveFieldPayloads field stateBody

-- | Pseudo-lens to an object inside a specified field
zoomFieldObject
    :: forall a field m struct
    . (MonadE m, ReplicaClock m, ReplicatedAsObject field)
    => UUID                     -- ^ Field name
    -> ObjectStateT field  m a  -- ^ Inner object modifier
    -> ObjectStateT struct m a
zoomFieldObject field innerModifier =
    errorContext ("ORSet.zoomFieldObject" <> show field) $ do
        StateChunk{stateBody} <- getObjectStateChunk
        let objectIds =
                [ objectId
                | AUuid objectId : _ <- filterAliveFieldPayloads field stateBody
                ]
        object <- case objectIds of
            []       -> Object <$> getEventUuid  -- create empty object
            oid:oids -> reduceObjectStates @field $ fmap Object $ oid :| oids
        lift $ runReaderT innerModifier object

-- | Create an ORSet object from a list of named fields.
newStruct
    :: (MonadState StateFrame m, ReplicaClock m)
    => [(UUID, Maybe (Instance Replicated))] -> m UUID
newStruct fields = do
    stateBody <-
        fmap fold $ for fields $ \(name, values) ->
            for (toList values) $ \(Instance value) -> do
                opId <- getEventUuid
                    -- TODO(2019-07-12, cblp, #15) sequential uuids
                valuePayload <- newRon value
                pure Op{opId, refId = Zero, payload = AUuid name : valuePayload}
    objectId <- getEventUuid
    modify' $
        (<>) $ Map.singleton objectId $
        StateChunk{stateType = setType, stateBody}
    pure objectId
