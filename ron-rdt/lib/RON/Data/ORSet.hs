{-# LANGUAGE ApplicativeDo #-}
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
    ORSet,
    ItemRef (..),
    ORSetMap,
    ORSetRep (..),
    addRef,
    addValue,
    findAnyAlive,
    findAnyAlive',
    getAliveItems,
    getValues,
    removeObjectIf,
    removeRef,
    removeValue,
    -- * struct_set
    addFieldValue,
    assignField,
    getFieldObject,
    newStruct,
    removeFieldValue,
    removeFieldValueIf,
    viewFieldLWW,
    viewFieldMax,
    viewFieldMin,
    viewFieldObject,
    viewFieldObjectMonoid,
    viewFieldSet,
) where

import           RON.Prelude

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible, Rep, Replicated, eqPayload,
                                    eqRef, fromPayload, getObjectRep,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk, toPayload)
import           RON.Error (MonadE, errorContext, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Store (MonadStore, loadObject', loadObjectChunk',
                            modifyObjectChunk_)
import           RON.Types (Atom (AUuid), ObjectRef (ObjectRef),
                            ObjectRefs (ObjectRefs),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk), UUID,
                            WireStateChunk (WireStateChunk, stateBody, stateType))
import           RON.Util (Instance (Instance))
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

-- | Untyped OR-Set.
-- Implementation: a map from the itemKey to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
newtype ORSetRep = ORSetRep (Map UUID Op)
    deriving (Eq, Show)

itemKey :: Op -> UUID
itemKey Op{opId, refId} = case refId of
    Zero -> opId   -- alive
    _    -> refId  -- tombstone

preferTombstone :: Op -> Op -> Op
preferTombstone = maxOn refId

instance Semigroup ORSetRep where
    ORSetRep set1 <> ORSetRep set2 =
        ORSetRep $ Map.unionWith preferTombstone set1 set2

instance Monoid ORSetRep where
    mempty = ORSetRep mempty

-- | Laws:
-- 1. Idempotent because 'Map.unionWith' is idempotent.
-- 2. Commutative because 'preferTombstone' is commutative.
instance Semilattice ORSetRep

instance Reducible ORSetRep where
    reducibleOpType = setType

    stateFromChunk ops = ORSetRep $
        Map.fromListWith preferTombstone [(itemKey op, op) | op <- ops]

    stateToChunk (ORSetRep set) = sortOn opId $ Map.elems set

wireStateChunk :: [Op] -> WireStateChunk
wireStateChunk stateBody = WireStateChunk{stateType = setType, stateBody}

-- | Name-UUID to use as OR-Set type marker.
setType :: UUID
setType = $(UUID.liftName "set")

-- | Type-directing tag for typed OR-Set.
data ORSet a

type instance Rep (ORSet a) = ORSetRep

-- TODO
-- newObject (ORSet items) = do
--     ops <- for items $ \item -> do
--         event <- getEventUuid
--         payload <- newRon item
--         pure $ Op event Zero payload
--     oid <- getEventUuid
--     modify' $ Map.insert oid $ wireStateChunk ops
--     pure $ ObjectRef oid

-- TODO
-- readObject = do
--     StateChunk ops <- getObjectStateChunk
--     mItems <- for ops $ \Op{refId, payload} -> case refId of
--         Zero -> do
--             item <- fromRon payload
--             pure $ Just item
--         _    -> pure Nothing
--     pure . ORSet $ catMaybes mItems

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
commonAdd ::
    (MonadE m, MonadStore m, ReplicaClock m) =>
    Payload -> ObjectRef (ORSet a) -> m ()
commonAdd payload =
    modifyObjectChunk_ $ \(StateChunk stateBody) -> do
        event <- getEventUuid
        pure $ StateChunk $ stateBody ++ [Op event Zero payload]

-- | Encode a value and add a it to the OR-Set
addValue ::
    (MonadE m, MonadStore m, ReplicaClock m, Replicated a) =>
    a -> ObjectRef (ORSet a) -> m ()
addValue = commonAdd . toPayload

addRef ::
    (MonadE m, MonadStore m, ReplicaClock m) =>
    ObjectRef a -> ObjectRef (ORSet a) -> m ()
addRef (ObjectRef itemUuid) = commonAdd [AUuid itemUuid]

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove ::
    (MonadE m, MonadStore m, ReplicaClock m) =>
    (Payload -> m Bool) -> ObjectRef (ORSet a) -> m ()
commonRemove isTarget =
    modifyObjectChunk_ $ \(StateChunk chunk) -> do
        let state0@(ORSetRep opMap) = stateFromChunk chunk
        targetEvents <-
            fmap catMaybes
            $ for (toList opMap) $ \op@Op{refId, payload} ->
                case refId of
                    Zero -> do
                        t <- isTarget payload
                        pure $ if t then Just op else Nothing
                    _ -> pure Nothing
        StateChunk <$>
            case targetEvents of
                [] -> pure chunk
                _  -> do
                    tombstone <- getEventUuid
                    let patch =
                            [ op{opId = tombstone, refId = observed}
                            | op@Op{opId = observed} <- targetEvents
                            ]
                    let state' = state0 <> stateFromChunk patch
                    pure $ stateToChunk state'

removeObjectIf ::
    (MonadE m, MonadStore m, ReplicaClock m) =>
    (ObjectRef a -> m Bool) -> ObjectRef (ORSet a) -> m ()
removeObjectIf isTarget = commonRemove $ \case
    AUuid uuid' : _ -> isTarget (ObjectRef uuid')
    _               -> pure False

-- | Remove an atomic value from the OR-Set
removeValue ::
    (MonadE m, MonadStore m, ReplicaClock m, Replicated a) =>
    a -> ObjectRef (ORSet a) -> m ()
removeValue v = commonRemove $ pure . eqPayload v

-- | Remove from a field all values that equal to the specified value.
removeFieldValue ::
    (   MonadE m,
        MonadStore m,
        Rep struct ~ ORSetRep,
        ReplicaClock m,
        Replicated a
        ) =>
    -- | Field name
    UUID ->
    -- | Value
    a ->
    ObjectRef struct ->
    m ()
removeFieldValue field value =
    removeFieldValueIfP field $ pure . eqPayload value

-- | Remove from a field all values that obey the predicate.
removeFieldValueIf ::
    (   MonadE m,
        MonadStore m,
        Rep struct ~ ORSetRep,
        ReplicaClock m,
        Replicated a
        ) =>
    -- | Field name
    UUID ->
    (a -> m Bool) ->
    ObjectRef struct ->
    m ()
removeFieldValueIf field isTarget =
    removeFieldValueIfP field $ \valuePayload -> do
        value <- fromPayload valuePayload
        isTarget value

-- | Remove from a field all payloads that obey the predicate.
removeFieldValueIfP ::
    (   MonadE m,
        MonadStore m,
        Rep struct ~ ORSetRep,
        ReplicaClock m
        ) =>
    -- | Field name
    UUID ->
    (Payload -> m Bool) ->
    ObjectRef struct ->
    m ()
removeFieldValueIfP field isTarget =
    modifyObjectChunk_ $ \(StateChunk stateBody) -> do
        (observedOps, stateBody1) <-
            partitionM isFieldAliveAndSameValue stateBody
        removeOps <- for observedOps $ \op@Op{opId = observedEvent} -> do
            tombstone <- getEventUuid  -- TODO(2019-07-10, cblp) sequential
            pure op{opId = tombstone, refId = observedEvent}
        let stateBody2 = stateBody1 ++ removeOps
        pure $ StateChunk stateBody2
    where
        isFieldAliveAndSameValue = \case
            Op{refId = Zero, payload = AUuid field' : valuePayload}
                | field' == field -> isTarget valuePayload
            _ -> pure False

-- | Remove an object reference from the OR-Set
removeRef ::
    (MonadE m, MonadStore m, ReplicaClock m) =>
    ObjectRef a -> ObjectRef (ORSet a) -> m ()
removeRef r = commonRemove $ pure . eqRef r

-- | Reference to an item inside an 'ORSet'.
newtype ItemRef a = ItemRef UUID
    deriving (Show)

-- | TODO? Go from modification of the whole set to the modification of an item
-- object.
-- zoomItem
--     :: MonadE m
--     => ItemRef item -> ObjectStateT item m a -> ObjectStateT (ORSet item) m a
-- zoomItem (ItemRef key) innerModifier = do
--     ORSetRep opMap <- getObjectRep
--     itemValueRef <- case Map.lookup key opMap of
--         Nothing ->
--             -- TODO(2019-07-08, cblp) create empty object?
--             throwErrorText "no such key in ORSet"
--         Just Op{payload} -> case payload of
--             [AUuid itemValueRef] -> pure itemValueRef
--             _ -> throwErrorText "item payload is not an object ref"
--     lift $ runReaderT innerModifier $ ObjectRef itemValueRef

-- | Find any alive item. If no alive item found, return 'Nothing'.
findAnyAlive ::
    (MonadE m, MonadStore m) => ObjectRef (ORSet a) -> m (Maybe (ItemRef a))
findAnyAlive ref = do
    ORSetRep opMap <- loadObject' ref
    let aliveItems = [op | op@Op{refId = Zero} <- toList opMap]
    pure $ case listToMaybe aliveItems of
        Nothing       -> Nothing
        Just Op{opId} -> Just $ ItemRef opId

-- | Find any alive item. If no alive item found, report an error.
findAnyAlive' ::
    (MonadE m, MonadStore m) => ObjectRef (ORSet a) -> m (ItemRef a)
findAnyAlive' ref = do
    mx <- findAnyAlive ref
    case mx of
        Just x  -> pure x
        Nothing -> throwErrorText "empty set"

type ORSetMap k v = ORSet (k, v)

-- | Assign a value to a field, deleting all previous (observed) values.
-- Assignment of 'Nothing' just deletes all values.
assignField ::
    (   MonadE m,
        MonadStore m,
        Rep struct ~ ORSetRep,
        ReplicaClock m,
        Replicated a
        ) =>
    -- | Field name
    UUID ->
    -- | Value
    Maybe a ->
    ObjectRef struct ->
    m ()
assignField field mvalue =
    modifyObjectChunk_ $ \(StateChunk stateBody) -> do
        addOp <- case mvalue of
            Just value -> do
                event <- getEventUuid
                pure $ Just Op
                    { opId = event
                    , refId = Zero
                    , payload = AUuid field : toPayload value
                    }
            Nothing -> pure Nothing
        let (observedOps, stateBody1) = partition (isAliveField field) stateBody
        removeOps <- for observedOps $ \op@Op{opId = observedEvent} -> do
            tombstone <- getEventUuid  -- TODO(2019-07-10, cblp) sequential
            pure op{opId = tombstone, refId = observedEvent}
        let stateBody2 = stateBody1 ++ toList addOp ++ removeOps
        pure $ StateChunk stateBody2

addFieldValue ::
    (   MonadE m,
        MonadStore m,
        Rep struct ~ ORSetRep,
        ReplicaClock m,
        Replicated a
        ) =>
    -- | Field name
    UUID ->
    -- | Value
    a ->
    ObjectRef struct ->
    m ()
addFieldValue field value =
    modifyObjectChunk_ $ \(StateChunk stateBody) -> do
        event <- getEventUuid
        let addOp = Op
                { opId = event
                , refId = Zero
                , payload = AUuid field : toPayload value
                }
        pure $ StateChunk $ stateBody ++ [addOp]

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

-- -- | Find object field, merge all versions, return 'Nothing' if no versions
-- getFieldObject ::
--     (   MonadE m,
--         MonadStore m,
--         Reducible (Rep a),
--         Rep struct ~ ORSetRep
--         ) =>
--     -- | Field name
--     UUID ->
--     ObjectRef struct ->
--     m (Maybe (ObjectRef a))
-- getFieldObject field structRef =
--     errorContext "ORSet.getFieldObject" $ do
--         StateChunk ops <- loadObjectChunk' structRef
--         let payloads = filterAliveFieldPayloads field ops
--             refs = [ref | AUuid ref : _ <- payloads]
--         case refs of
--             []   -> pure Nothing
--             p:ps -> fmap Just $ reduceObjectStates $ fmap ObjectRef $ p :| ps

-- | Get reference to the object from the field
-- (multiple references if multiple versions)
viewFieldObject
    :: UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> ObjectRefs a
viewFieldObject field (StateChunk stateBody) =
    ObjectRefs $
        HashSet.fromList
            [ref | AUuid ref : _ <- filterAliveFieldPayloads field stateBody]

viewFieldObjectMonoid ::
    (MonadE m, MonadStore m, Reducible (Rep a)) =>
    -- | Field name
    UUID ->
    -- | ORSet object chunk
    StateChunk ORSetRep ->
    m (Maybe (ObjectRef a))
viewFieldObjectMonoid field chunk =
    case toList refs of
        [] -> pure Nothing
        r : rs ->
            errorContext "ORSet.viewFieldObjectMonoid" $
                Just <$> reduceObjectStates (ObjectRef <$> (r :| rs))
    where
        ObjectRefs refs = viewFieldObject field chunk

-- | Decode field value, keep last version only
viewFieldLWW
    :: (MonadE m, Replicated a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldLWW field (StateChunk stateBody) =
    errorContext "ORSet.viewFieldLWW" $ do
    let mPayload =
            fmap snd . maximumMayOn fst $
            filterAliveFieldIdsAndPayloads field stateBody
    case mPayload of
        Nothing      -> pure Nothing
        Just []      -> pure Nothing
        Just payload -> Just <$> fromPayload payload

-- | Decode field value, keep max value only, only for Integer and Float
viewFieldMax
    :: (MonadE m, Ord a, Replicated a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldMax field (StateChunk stateBody) =
    errorContext "ORSet.viewFieldMax" $
    fmap maximumMay $
    traverse fromPayload $
    filterAliveFieldPayloads field stateBody

-- | Decode field value, keep min value only, only for Integer and Float
viewFieldMin
    :: (MonadE m, Ord a, Replicated a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m (Maybe a)
viewFieldMin field (StateChunk stateBody) =
    errorContext "ORSet.viewFieldMin" $
    fmap minimumMay $
    traverse fromPayload $
    filterAliveFieldPayloads field stateBody

-- | Decode field value, keep all versions
viewFieldSet
    :: (MonadE m, Replicated a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m [a]
viewFieldSet field (StateChunk stateBody) =
    errorContext "ORSet.viewFieldSet" $
    traverse fromPayload $
    filterAliveFieldPayloads field stateBody

-- | Create an ORSet object from a list of named fields.
newStruct
    :: (MonadStore m, ReplicaClock m)
    => [(UUID, Instance Replicated)] -> m UUID
newStruct fields = do
    objectId <- getEventUuid
    stateBody <-
        for fields $ \(name, Instance value) -> do
            opId <- getEventUuid -- TODO(2019-07-12, cblp, #15) sequential uuids
            pure Op{opId, refId = Zero, payload = AUuid name : toPayload value}
    modify' $ Map.insert objectId $ wireStateChunk stateBody
    pure objectId

partitionM :: Applicative m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = \case
    [] -> pure ([], [])
    x:xs -> do
        res <- f x
        (as, bs) <- partitionM f xs
        pure ([x | res] ++ as, [x | not res] ++ bs)

-- | Get alive items as list.
getAliveItems ::
    (MonadE m, MonadStore m) => ObjectRef (ORSet a) -> m [ItemRef a]
getAliveItems ref = do
    ORSetRep opMap <- getObjectRep ref
    pure [ItemRef opId | Op{refId = Zero, opId} <- toList opMap]

-- | Get values as list. May contain duplicates.
-- User should deal with duplicates with 'Set' or 'HashSet'.
getValues ::
    (MonadE m, MonadStore m, Replicated a) =>
    ObjectRef (ORSet a) -> m [a]
getValues ref = do
    ORSetRep opMap <- getObjectRep ref
    traverse fromPayload [payload | Op{refId = Zero, payload} <- toList opMap]
