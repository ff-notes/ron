{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
                                    Replicated, fromRon, getObjectStateChunk,
                                    modifyObjectStateChunk_, newRon,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Types (Atom (AUuid), Object (..), Op (..), StateChunk (..),
                            StateFrame, UUID)
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

    stateToChunk (LwwRep fields) = mkStateChunk $ Map.elems fields

mkStateChunk :: [Op] -> StateChunk
mkStateChunk stateBody = StateChunk{stateType = lwwType, stateBody}

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
    modify' $
        (<>) $ Map.singleton event $ StateChunk{stateType = lwwType, stateBody}
    pure event

-- | Decode field value
viewField
    :: (Replicated a, MonadE m, MonadState StateFrame m)
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ LWW object chunk
    -> m (Maybe a)
viewField field StateChunk{stateBody} =
    errorContext "LWW.viewField" $ do
        let mPayload =
                fmap payload $
                maximumMayOn opId $
                filter (\Op{refId} -> refId == field) stateBody
        case mPayload of
            Nothing      -> pure Nothing
            Just []      -> pure Nothing
            Just payload -> Just <$> fromRon payload

-- | Read field value
readField
    :: (MonadE m, MonadObjectState struct m, Replicated field)
    => UUID  -- ^ Field name
    -> m (Maybe field)
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
    modifyObjectStateChunk_ $ \StateChunk{stateBody} -> do
        let chunk = filter (\Op{refId} -> refId /= field) stateBody
        event <- getEventUuid
        p <- maybe (pure []) newRon mvalue
        let newOp = Op event field p
        let chunk' = sortOn refId $ newOp : chunk
        pure StateChunk{stateBody = chunk', stateType = lwwType}

-- | Pseudo-lens to an object inside a specified field
zoomField
    :: MonadE m
    => UUID                     -- ^ Field name
    -> ObjectStateT field  m a  -- ^ Inner object modifier
    -> ObjectStateT struct m a
zoomField field innerModifier =
    errorContext ("LWW.zoomField" <> show field) $ do
        StateChunk{stateBody} <- getObjectStateChunk
        let ops = filter (\Op{refId} -> refId == field) stateBody
        Op{payload} <- case ops of
            []   -> throwError "empty chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        fieldObjectId <- errorContext "inner object" $ case payload of
            [AUuid oid] -> pure oid
            _           -> throwError "Expected object UUID"
        lift $ runReaderT innerModifier $ Object fieldObjectId
