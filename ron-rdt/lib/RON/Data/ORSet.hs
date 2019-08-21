{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    ORSetRep (..),
    addRef,
    addValue,
    findAnyAlive,
    findAnyAlive',
    removeObjectIf,
    removeRef,
    removeValue,
    zoomItem,
    -- * struct_set
    addFieldValue,
    assignField,
    getFieldObject,
    newStruct,
    removeFieldValue,
    removeFieldValueIf,
    viewField,
    viewFieldLWW,
    viewFieldMax,
    viewFieldMin,
    viewFieldSet,
    zoomFieldObject,
) where

import           RON.Prelude

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           RON.Data.Internal (MonadObjectState, ObjectStateT, Reducible,
                                    Rep, Replicated (encoding),
                                    ReplicatedAsObject,
                                    ReplicatedAsPayload (fromPayload),
                                    eqPayload, eqRef, evalObjectState, fromRon,
                                    getObjectState, getObjectStateChunk,
                                    modifyObjectStateChunk_, newObject, newRon,
                                    objectEncoding, rconcat, readObject,
                                    reduceObjectStates, reducibleOpType,
                                    stateFromChunk, stateToChunk)
import           RON.Error (MonadE, errorContext, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Types (Atom (AUuid),
                            ObjectFrame (ObjectFrame, frame, uuid),
                            ObjectRef (ObjectRef),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk), StateFrame, UUID,
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

-- | Type-directing wrapper for typed OR-Set.
data ORSet c a where
    ORSetOrd  :: Set     a -> ORSet Ord      a
    ORSetHash :: HashSet a -> ORSet Hashable a

instance (Ord a, Replicated a) => Replicated (ORSet Ord a) where
    encoding = objectEncoding

instance (Eq a, Hashable a, Replicated a) => Replicated (ORSet Hashable a) where
    encoding = objectEncoding

instance (Ord a, Replicated a) => ReplicatedAsObject (ORSet Ord a) where
    type Rep (ORSet Ord a) = ORSetRep

    newObject (ORSetOrd items) = newObject' $ toList items

    readObject = ORSetOrd . Set.fromList <$> readObject'

instance
    (Eq a, Hashable a, Replicated a) => ReplicatedAsObject (ORSet Hashable a)
    where

    type Rep (ORSet Hashable a) = ORSetRep

    newObject (ORSetHash items) = newObject' $ toList items

    readObject = ORSetHash . HashSet.fromList <$> readObject'

newObject'
    :: (MonadState StateFrame m, ReplicaClock m, Replicated a)
    => [a] -> m (ObjectRef (ORSet c a))
newObject' items = do
    ops <- for items $ \item -> do
        event <- getEventUuid
        payload <- newRon item
        pure $ Op event Zero payload
    oid <- getEventUuid
    modify' $ Map.insert oid $ wireStateChunk ops
    pure $ ObjectRef oid

readObject' :: (MonadE m, MonadObjectState (ORSet c a) m, Replicated a) => m [a]
readObject' = do
    StateChunk ops <- getObjectStateChunk
    fmap catMaybes . for ops $ \Op{refId, payload} -> case refId of
        Zero -> do
            item <- fromRon payload
            pure $ Just item
        _ -> pure Nothing

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
commonAdd :: (MonadE m, MonadObjectState a m, ReplicaClock m) => Payload -> m ()
commonAdd payload =
    modifyObjectStateChunk_ $ \(StateChunk stateBody) -> do
        event <- getEventUuid
        pure $ StateChunk $ stateBody ++ [Op event Zero payload]

-- | Encode a value and add a it to the OR-Set
addValue
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState (ORSet c a) m)
    => a -> m ()
addValue item = do
    payload <- newRon item
    commonAdd payload

addRef
    :: (ReplicaClock m, MonadE m, MonadObjectState (ORSet c a) m)
    => ObjectRef a -> m ()
addRef (ObjectRef itemUuid) = commonAdd [AUuid itemUuid]

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet c a) m)
    => (Payload -> m Bool) -> m ()
commonRemove isTarget =
    modifyObjectStateChunk_ $ \(StateChunk chunk) -> do
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

removeObjectIf
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet c a) m)
    => ObjectStateT a m Bool -> m ()
removeObjectIf isTarget = commonRemove $ \case
    AUuid uuid' : _ -> do
        frame <- get
        evalObjectState ObjectFrame{uuid = uuid', frame} isTarget
    _ -> pure False

-- | Remove an atomic value from the OR-Set
removeValue
    ::  ( ReplicatedAsPayload a
        , MonadE m, ReplicaClock m, MonadObjectState (ORSet c a) m
        )
    => a -> m ()
removeValue v = commonRemove $ pure . eqPayload v

-- | Remove from a field all values that equal to the specified value.
removeFieldValue
    ::  ( MonadE m
        , MonadObjectState struct m
        , ReplicaClock m
        , ReplicatedAsPayload a
        )
    => UUID -- ^ Field name
    -> a -- ^ Value
    -> m ()
removeFieldValue field value =
    removeFieldValueIfP field $ pure . eqPayload value

-- | Remove from a field all values that obey the predicate.
removeFieldValueIf
    ::  ( MonadE m
        , MonadObjectState struct m
        , ReplicaClock m
        , ReplicatedAsPayload a
        )
    => UUID -- ^ Field name
    -> (a -> m Bool)
    -> m ()
removeFieldValueIf field isTarget =
    removeFieldValueIfP field $ \valuePayload -> do
        value <- fromPayload valuePayload
        isTarget value

-- | Remove from a field all payloads that obey the predicate.
removeFieldValueIfP
    :: (MonadE m, MonadObjectState struct m, ReplicaClock m)
    => UUID -- ^ Field name
    -> (Payload -> m Bool)
    -> m ()
removeFieldValueIfP field isTarget =
    modifyObjectStateChunk_ $ \(StateChunk stateBody) -> do
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
removeRef
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet c a) m)
    => ObjectRef a -> m ()
removeRef r = commonRemove $ pure . eqRef r

-- | Reference to an item inside an 'ORSet'.
newtype ORSetItem a = ORSetItem UUID
    deriving (Show)

-- | Go from modification of the whole set to the modification of an item
-- object.
zoomItem
    :: (MonadE m, Rep (ORSet c item) ~ ORSetRep)
    => ORSetItem item
    -> ObjectStateT item m a
    -> ObjectStateT (ORSet c item) m a
zoomItem (ORSetItem key) innerModifier = do
    ORSetRep opMap <- getObjectState
    itemValueRef <- case Map.lookup key opMap of
        Nothing ->
            -- TODO(2019-07-08, cblp) create empty object?
            throwErrorText "no such key in ORSet"
        Just Op{payload} -> case payload of
            [AUuid itemValueRef] -> pure itemValueRef
            _ -> throwErrorText "item payload is not an object ref"
    lift $ runReaderT innerModifier $ ObjectRef itemValueRef

-- | Find any alive item. If no alive item found, return 'Nothing'.
findAnyAlive
    ::  ( MonadE m
        , MonadObjectState (ORSet c item) m
        , Rep (ORSet c item) ~ ORSetRep
        )
    =>  m (Maybe (ORSetItem item))
findAnyAlive = do
    ORSetRep opMap <- getObjectState
    let aliveItems = [op | op@Op{refId = Zero} <- toList opMap]
    pure $ case listToMaybe aliveItems of
        Nothing       -> Nothing
        Just Op{opId} -> Just $ ORSetItem opId

-- | Find any alive item. If no alive item found, report an error.
findAnyAlive'
    ::  ( MonadE m
        , MonadObjectState (ORSet c item) m
        , Rep (ORSet c item) ~ ORSetRep
        )
    =>  m (ORSetItem item)
findAnyAlive' = do
    mx <- findAnyAlive
    case mx of
        Just x  -> pure x
        Nothing -> throwErrorText "empty set"

type ORSetMap c k v = ORSet c (k, v)

-- | Assign a value to a field, deleting all previous (observed) values.
-- Assignment of 'Nothing' just deletes all values.
assignField
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID     -- ^ Field name
    -> Maybe a  -- ^ Value
    -> m ()
assignField field mvalue =
    modifyObjectStateChunk_ $ \(StateChunk stateBody) -> do
        addOp <- case mvalue of
            Just value -> do
                event <- getEventUuid
                valuePayload <- newRon value
                pure $ Just Op
                    { opId = event
                    , refId = Zero
                    , payload = AUuid field : valuePayload
                    }
            Nothing -> pure Nothing
        let (observedOps, stateBody1) = partition (isAliveField field) stateBody
        removeOps <- for observedOps $ \op@Op{opId = observedEvent} -> do
            tombstone <- getEventUuid  -- TODO(2019-07-10, cblp) sequential
            pure op{opId = tombstone, refId = observedEvent}
        let stateBody2 = stateBody1 ++ toList addOp ++ removeOps
        pure $ StateChunk stateBody2

addFieldValue
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID  -- ^ Field name
    -> a     -- ^ Value
    -> m ()
addFieldValue field value =
    modifyObjectStateChunk_ $ \(StateChunk stateBody) -> do
        event <- getEventUuid
        valuePayload <- newRon value
        let addOp = Op
                { opId = event
                , refId = Zero
                , payload = AUuid field : valuePayload
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

-- | Find object field, merge all versions, return 'Nothing' if no versions
getFieldObject
    :: (MonadE m, MonadObjectState struct m, ReplicatedAsObject a)
    => UUID                 -- ^ Field name
    -> m (Maybe (ObjectRef a))
getFieldObject field =
    errorContext "ORSet.getFieldObject" $ do
        StateChunk ops <- getObjectStateChunk
        let payloads = filterAliveFieldPayloads field ops
            refs = [ref | AUuid ref : _ <- payloads]
        case refs of
            []   -> pure Nothing
            p:ps -> fmap Just $ reduceObjectStates $ fmap ObjectRef $ p :| ps

-- | Decode field value, merge all versions, return 'Nothing' if no versions
viewField
    :: (MonadE m, MonadState StateFrame m, ReplicatedAsObject a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m (Maybe a)
viewField field (StateChunk stateBody) =
    errorContext "ORSet.viewField" $ do
        let payloads = filterAliveFieldPayloads field stateBody
            refs = [ref | AUuid ref : _ <- payloads]
        case refs of
            []   -> pure Nothing
            p:ps -> fmap Just . rconcat $ p :| ps

-- | Decode field value, keep last version only
viewFieldLWW
    :: (MonadE m, MonadState StateFrame m, Replicated a)
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
        Just payload -> Just <$> fromRon payload

-- | Decode field value, keep max value only, only for Integer and Float
viewFieldMax
    :: (MonadE m, Ord a, ReplicatedAsPayload a)
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
    :: (MonadE m, Ord a, ReplicatedAsPayload a)
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
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => UUID                 -- ^ Field name
    -> StateChunk ORSetRep  -- ^ ORSet object chunk
    -> m [a]
viewFieldSet field (StateChunk stateBody) =
    errorContext "ORSet.viewFieldSet" $
    traverse fromRon $
    filterAliveFieldPayloads field stateBody

-- | Pseudo-lens to an object inside a specified field
zoomFieldObject
    ::  forall a field m struct
    .   ( MonadE m
        , ReplicaClock m
        , ReplicatedAsObject field
        , ReplicatedAsObject struct
        )
    =>  UUID                     -- ^ Field name
    ->  ObjectStateT field  m a  -- ^ Inner object modifier
    ->  ObjectStateT struct m a
zoomFieldObject field innerModifier =
    errorContext ("ORSet.zoomFieldObject " <> show field) $ do
        (StateChunk stateBody) <- getObjectStateChunk
        let objectIds =
                [ objectId
                | AUuid objectId : _ <- filterAliveFieldPayloads field stateBody
                ]
        object <- case objectIds of
            []       -> ObjectRef <$> getEventUuid  -- create empty object
            oid:oids -> reduceObjectStates @field $ fmap ObjectRef $ oid :| oids
        lift $ runReaderT innerModifier object

-- | Create an ORSet object from a list of named fields.
newStruct
    :: (MonadState StateFrame m, ReplicaClock m)
    => [(UUID, Instance Replicated)] -> m UUID
newStruct fields = do
    objectId <- getEventUuid
    stateBody <-
        for fields $ \(name, Instance value) -> do
            opId <- getEventUuid -- TODO(2019-07-12, cblp, #15) sequential uuids
            valuePayload <- newRon value
            pure Op{opId, refId = Zero, payload = AUuid name : valuePayload}
    modify' $ Map.insert objectId $ wireStateChunk stateBody
    pure objectId

partitionM :: Applicative m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = \case
    [] -> pure ([], [])
    x:xs -> do
        res <- f x
        (as, bs) <- partitionM f xs
        pure ([x | res] ++ as, [x | not res] ++ bs)
