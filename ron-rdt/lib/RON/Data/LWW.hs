{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Control.Error (fmapL)
import           Control.Monad.Except (MonadError, liftEither, throwError)
import           Control.Monad.State.Strict (MonadState, StateT, get, put,
                                             runStateT)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)

import           RON.Data.Internal (Reducible, Replicated, ReplicatedAsObject,
                                    collectFrame, fromRon, getObjectStateChunk,
                                    mkStateChunk, newRon, objectOpType,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Types (Atom (AUuid), Object (..), Op (..), StateChunk (..),
                            StateFrame, UUID)
import           RON.Util (Instance (Instance), maxOn)
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

    stateToChunk (LwwPerField fields) = mkStateChunk $ Map.elems fields

-- | Name-UUID to use as LWW type marker.
lwwType :: UUID
lwwType = $(UUID.liftName "lww")

-- | Create LWW object from a list of named fields.
newObject :: ReplicaClock m => [(UUID, Instance Replicated)] -> m (Object a)
newObject fields = collectFrame $ do
    payloads <- for fields $ \(_, Instance value) -> newRon value
    e <- lift getEventUuid
    tell $ Map.singleton (lwwType, e) $ StateChunk e
        [Op e name p | ((name, _), p) <- zip fields payloads]
    pure e

-- | Decode field value
viewField
    :: Replicated a
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ LWW object chunk
    -> StateFrame
    -> Either String a
viewField field StateChunk{..} frame =
    fmapL (("LWW.viewField " <> show field <> ":\n") <>) $ do
        let ops = filter ((field ==) . opRef) stateBody
        Op{..} <- case ops of
            []   -> Left $ unwords ["no field", show field, "in lww chunk"]
            [op] -> pure op
            _    -> Left "unreduced state"
        fromRon opPayload frame

-- | Decode field value
readField
    ::  ( MonadError String m
        , MonadState (Object a) m
        , ReplicatedAsObject a
        , Replicated b
        )
    => UUID  -- ^ Field name
    -> m b
readField field = do
    obj@Object{..} <- get
    liftEither $ do
        stateChunk <- getObjectStateChunk obj
        viewField field stateChunk objectFrame

-- | Assign a value to a field
assignField
    :: forall a b m
    .   ( ReplicatedAsObject a
        , Replicated b
        , ReplicaClock m, MonadError String m, MonadState (Object a) m
        )
    => UUID  -- ^ Field name
    -> b     -- ^ Value (from untyped world)
    -> m ()
assignField field value = do
    obj@Object{..} <- get
    StateChunk{..} <- liftEither $ getObjectStateChunk obj
    advanceToUuid stateVersion
    let chunk = filter ((field /=) . opRef) stateBody
    e <- getEventUuid
    (p, frame') <- runWriterT $ newRon value
    let newOp = Op e field p
    let chunk' = sortOn opRef $ newOp : chunk
    let state' = StateChunk e chunk'
    put Object
        { objectFrame =
            Map.insert (objectOpType @a, objectId) state' objectFrame <> frame'
        , ..
        }

-- | Anti-lens to an object inside a specified field
zoomField
    :: (ReplicatedAsObject outer, MonadError String m)
    => UUID                       -- ^ Field name
    -> StateT (Object inner) m a  -- ^ Nested object modifier
    -> StateT (Object outer) m a
zoomField field innerModifier = do
    obj@Object{..} <- get
    StateChunk{..} <- liftEither $ getObjectStateChunk obj
    let ops = filter ((field ==) . opRef) stateBody
    Op{..} <- case ops of
        []   -> throwError $ unwords ["no field", show field, "in lww chunk"]
        [op] -> pure op
        _    -> throwError "unreduced state"
    innerObjectId <- case opPayload of
        [AUuid oid] -> pure oid
        _           -> throwError "bad payload"
    let innerObject = Object innerObjectId objectFrame
    (a, Object{objectFrame = objectFrame'}) <-
        lift $ runStateT innerModifier innerObject
    put Object{objectFrame = objectFrame', ..}
    pure a
