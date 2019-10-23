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
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible, Rep, Replicated, reducibleOpType,
                                    stateFromChunk, stateToChunk, toPayload,
                                    tryOptionFromRon)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Store (MonadStore)
import           RON.Types (Atom, ObjectRef, Op (Op, opId, payload, refId),
                            Payload, StateChunk (StateChunk), UUID,
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

-- | Decode field value
viewField
    :: (Replicated a, MonadE m)
    => UUID               -- ^ Field name
    -> StateChunk LwwRep  -- ^ LWW object chunk
    -> m (Maybe a)
viewField field (StateChunk ops) =
    errorContext "LWW.viewField" $
    maybe (pure Nothing) (tryOptionFromRon . payload) $
    maximumMayOn opId $
    filter (\Op{refId} -> refId == field) ops

-- | Read field value
readField ::
    (   MonadE m,
        MonadState (StateChunk LwwRep) m,
        Replicated field
        ) =>
    -- | Key/field name
    Atom ->
    m (Maybe field)
readField field = do
    stateChunk <- get
    viewField field stateChunk

-- | Assign a value to a field
assignField ::
    (MonadStore m, Rep a ~ LwwRep) =>
    -- | Key/field name
    Atom ->
    -- | Value
    Payload ->
    -- | Object
    ObjectRef a ->
    m ()
assignField field mvalue =
    undefined
    -- modifyObjectStateChunk_ $ \(StateChunk ops) -> do
    --     let chunk = filter (\Op{refId} -> refId /= field) ops
    --     event <- getEventUuid
    --     let payload = maybe [] toPayload mvalue
    --     let newOp = Op event field payload
    --     pure $ StateChunk $ sortOn refId $ newOp : chunk
