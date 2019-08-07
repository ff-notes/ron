{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | LWW-per-field RDT
module RON.Data.LWW (
    LwwRep (..),
    assignField,
    lwwType,
    newStruct,
    readField,
    viewField,
    zoomField,
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (MonadObjectState, ObjectStateT, Reducible,
                                    Rep, Replicated, ReplicatedAsObject,
                                    getObjectStateChunk,
                                    modifyObjectStateChunk_, newRon,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk, tryOptionFromRon)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Types (Atom (AUuid), Object (Object),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk), StateFrame, UUID,
                            WireStateChunk (WireStateChunk, stateBody, stateType))
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

-- | Last-Write-Wins: select an op with latter event
lww :: Op -> Op -> Op
lww = maxOn opId

-- | Untyped LWW. Implementation: a map from 'opRef' to the original op.
newtype LwwRep = LwwRep (Map UUID Op)
    deriving (Eq, Monoid, Show)

instance Semigroup LwwRep where
    LwwRep fields1 <> LwwRep fields2 =
        LwwRep $ Map.unionWith lww fields1 fields2

-- | Laws:
-- 1. Idempotent because 'Map.unionWith' is idempotent.
-- 2. Commutative because 'lww' is commutative.
instance Semilattice LwwRep

instance Reducible LwwRep where
    reducibleOpType = lwwType

    stateFromChunk ops =
        LwwRep $ Map.fromListWith lww [(refId, op) | op@Op{refId} <- ops]

    stateToChunk (LwwRep fields) = Map.elems fields

wireStateChunk :: [Op] -> WireStateChunk
wireStateChunk stateBody = WireStateChunk{stateType = lwwType, stateBody}

-- | Name-UUID to use as LWW type marker.
lwwType :: UUID
lwwType = $(UUID.liftName "lww")

-- | Create an LWW object from a list of named fields.
newStruct
    :: (MonadState StateFrame m, ReplicaClock m)
    => [(UUID, Maybe (Instance Replicated))] -> m UUID
newStruct fields = do
    event <- getEventUuid
    stateBody <-
        for fields $ \(name, mvalue) -> do
            payload <- case mvalue of
                Just (Instance value) -> newRon value
                Nothing               -> pure []
            pure $ Op event name payload
    modify' $ Map.insert event $ wireStateChunk stateBody
    pure event

-- | Decode field value
viewField
    :: (Replicated a, MonadE m, MonadState StateFrame m)
    => UUID               -- ^ Field name
    -> StateChunk LwwRep  -- ^ LWW object chunk
    -> m (Maybe a)
viewField field (StateChunk ops) =
    errorContext "LWW.viewField" $
    maybe (pure Nothing) tryOptionFromRon $
    fmap payload $
    maximumMayOn opId $
    filter (\Op{refId} -> refId == field) ops

-- | Read field value
readField
    ::  ( MonadE m
        , MonadObjectState struct m
        , Rep struct ~ LwwRep
        , Replicated field
        )
    =>  UUID  -- ^ Field name
    ->  m (Maybe field)
readField field = do
    stateChunk <- getObjectStateChunk
    viewField field stateChunk

-- | Assign a value to a field
assignField
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID     -- ^ Field name
    -> Maybe a  -- ^ Value
    -> m ()
assignField field mvalue =
    modifyObjectStateChunk_ $ \(StateChunk ops) -> do
        let chunk = filter (\Op{refId} -> refId /= field) ops
        event <- getEventUuid
        p <- maybe (pure []) newRon mvalue
        let newOp = Op event field p
        pure $ StateChunk $ sortOn refId $ newOp : chunk

-- | Pseudo-lens to an object inside a specified field
zoomField
    :: (MonadE m, ReplicatedAsObject struct)
    => UUID                     -- ^ Field name
    -> ObjectStateT field  m a  -- ^ Inner object modifier
    -> ObjectStateT struct m a
zoomField field innerModifier =
    errorContext ("LWW.zoomField" <> show field) $ do
        StateChunk ops <- getObjectStateChunk
        let fieldOps = filter (\Op{refId} -> refId == field) ops
        Op{payload} <- case fieldOps of
            []   -> throwError "empty chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        fieldObjectId <- errorContext "inner object" $ case payload of
            [AUuid oid] -> pure oid
            _           -> throwError "Expected object UUID"
        lift $ runReaderT innerModifier $ Object fieldObjectId
