{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField (..)
    , getField
    , lwwType
    , modifyField
    , newFrame
    , writeField
    ) where

import           RON.Internal.Prelude

import           Control.Error (fmapL)
import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (MonadState, StateT, execStateT,
                                             get, put)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), ReplicatedAsObject,
                                    ReplicatedAsPayload (..), collectFrame,
                                    getObjectStateChunk, mkStateChunk,
                                    objectOpType)
import           RON.Event (Clock, advanceToUuid, getEventUuid)
import           RON.Types (Atom (AUuid), Frame', Object (..), Op' (..),
                            StateChunk (..), UUID)
import qualified RON.UUID as UUID

lww :: Op' -> Op' -> Op'
lww = maxOn opEvent

-- | Key is 'opRef', value is the original op
newtype LwwPerField = LwwPerField (Map UUID Op')
    deriving (Eq, Monoid, Show)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    type OpType LwwPerField = "lww"

    stateFromChunk ops =
        LwwPerField $ Map.fromListWith lww [(opRef op, op) | op <- ops]

    stateToChunk (LwwPerField fields) = mkStateChunk $ Map.elems fields

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

newFrame
    :: (Clock clock, ReplicatedAsObject a)
    => [(UUID, I ReplicatedAsPayload)] -> clock (Object a)
newFrame fields = collectFrame $ do
    payloads <- for fields $ \(_, I value) -> newPayload value
    e <- lift getEventUuid
    tell $ Map.singleton (lwwType, e) $ StateChunk e
        [Op' e name p | ((name, _), p) <- zip fields payloads]
    pure e

getField
    :: ReplicatedAsPayload a
    => UUID -> StateChunk -> Frame' -> Either String a
getField field StateChunk{..} frame = fmapL ("getField:\n" <>) $ do
    let ops = filter ((field ==) . opRef) stateBody
    Op'{..} <- case ops of
        []   -> Left $ unwords ["no field", show field, "in lww chunk"]
        [op] -> pure op
        _    -> Left "unreduced state"
    fromPayload opPayload frame

writeField
    :: (Clock m, MonadError String m, MonadState (Object a) m)
    => UUID
    -> I ReplicatedAsPayload
    -> m ()
writeField field (I value) = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    advanceToUuid stateVersion
    let chunk = filter ((field /=) . opRef) stateBody
    e <- getEventUuid
    (p, frame') <- runWriterT $ newPayload value
    let newOp = Op' e field p
    let chunk' = sortOn opRef $ newOp : chunk
    let state' = StateChunk e chunk'
    put Object
        { objectFrame = Map.insert objectId state' objectFrame <> frame'
        , ..
        }

modifyField
    :: forall inner outer m
    . (ReplicatedAsObject inner, MonadError String m)
    => UUID -> StateT (Object inner) m () -> StateT (Object outer) m ()
modifyField field innerModifier = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    let ops = filter ((field ==) . opRef) stateBody
    Op'{..} <- case ops of
        []   -> throwError $ unwords ["no field", show field, "in lww chunk"]
        [op] -> pure op
        _    -> throwError "unreduced state"
    innerObjectId <- case opPayload of
        [AUuid oid] -> pure oid
        _           -> throwError "bad payload"
    let innerObject = Object (objectOpType @inner, innerObjectId) objectFrame
    Object{objectFrame = objectFrame'} <-
        lift $ execStateT innerModifier innerObject
    put Object{objectFrame = objectFrame', ..}
