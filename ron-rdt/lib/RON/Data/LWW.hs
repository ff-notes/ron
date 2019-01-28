{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | LWW-per-field RDT
module RON.Data.LWW
    ( LwwPerField (..)
    , assignField
    , lwwType
    , newObject
    , readField
    , viewField
    , zoomField
    ) where

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible, Replicated, collectFrame,
                                    fromRon, getObjectStateChunk, mkStateChunk,
                                    newRon, reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Types (Atom (AUuid), Object (..), Op (..), StateChunk (..),
                            StateFrame, UUID)
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

-- | Last-Write-Wins: select an op with latter event
lww :: Op -> Op -> Op
lww = maxOn opEvent

-- | Untyped LWW. Implementation: a map from 'opRef' to the original op.
newtype LwwPerField = LwwPerField (Map UUID Op)
    deriving (Eq, Monoid, Show)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    reducibleOpType = lwwType

    stateFromChunk ops =
        LwwPerField $ Map.fromListWith lww [(opRef op, op) | op <- ops]

    stateToChunk (LwwPerField fields) = mkStateChunk lwwType $ Map.elems fields

-- | Name-UUID to use as LWW type marker.
lwwType :: UUID
lwwType = $(UUID.liftName "lww")

-- | Create LWW object from a list of named fields.
newObject :: ReplicaClock m => [(UUID, Instance Replicated)] -> m (Object a)
newObject fields = collectFrame $ do
    payloads <- for fields $ \(_, Instance value) -> newRon value
    event <- lift getEventUuid
    tell $
        Map.singleton event $
        StateChunk
            { stateType = lwwType
            , stateVersion = event
            , stateBody =
                [Op event name p | ((name, _), p) <- zip fields payloads]
            }
    pure event

-- | Decode field value
viewField
    :: (Replicated a, MonadE m)
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ LWW object chunk
    -> StateFrame
    -> m a
viewField field StateChunk{..} frame =
    errorContext ("LWW.viewField " <> show field) $ do
        let ops = filter ((field ==) . opRef) stateBody
        Op{..} <- case ops of
            []   -> throwError "no field in lww chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        fromRon opPayload frame

-- | Decode field value
readField
    :: (MonadE m, MonadState (Object a) m, Replicated b)
    => UUID  -- ^ Field name
    -> m b
readField field = do
    obj@Object{..} <- get
    stateChunk <- getObjectStateChunk obj
    viewField field stateChunk frame

-- | Assign a value to a field
assignField
    :: forall a b m
    . (Replicated b, ReplicaClock m, MonadE m, MonadState (Object a) m)
    => UUID  -- ^ Field name
    -> b     -- ^ Value (from untyped world)
    -> m ()
assignField field value = do
    obj@Object{id, frame} <- get
    StateChunk{..} <- getObjectStateChunk obj
    advanceToUuid stateVersion
    let chunk = filter ((field /=) . opRef) stateBody
    event <- getEventUuid
    (p, frame') <- runWriterT $ newRon value
    let newOp = Op event field p
    let chunk' = sortOn opRef $ newOp : chunk
    let state' = StateChunk
            {stateVersion = event, stateBody = chunk', stateType = lwwType}
    put obj{frame = Map.insert id state' frame <> frame'}

-- | Anti-lens to an object inside a specified field
zoomField
    :: MonadE m
    => UUID                       -- ^ Field name
    -> StateT (Object inner) m a  -- ^ Nested object modifier
    -> StateT (Object outer) m a
zoomField field innerModifier =
    errorContext ("LWW.zoomField" <> show field) $ do
        obj@Object{..} <- get
        StateChunk{..} <- getObjectStateChunk obj
        let ops = filter ((field ==) . opRef) stateBody
        Op{..} <- case ops of
            []   -> throwError "empty chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        innerObjectId <- errorContext "inner object" $ case opPayload of
            [AUuid oid] -> pure oid
            _           -> throwError "Expected object UUID"
        let innerObject = Object innerObjectId frame
        (a, Object{frame = frame'}) <-
            lift $ runStateT innerModifier innerObject
        put obj{frame = frame'}
        pure a
