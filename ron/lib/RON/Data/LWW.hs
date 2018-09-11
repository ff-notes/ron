{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField (..)
    , getLwwField
    , lwwType
    , newLwwFrame
    , writeLwwField
    ) where

import           RON.Internal.Prelude

import           Control.Error (fmapL)
import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (MonadState, get, put)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), ReplicatedAsObject,
                                    ReplicatedAsPayload (..), collectFrame,
                                    getObjectStateChunk, mkStateChunk)
import           RON.Event (Clock, advanceToUuid, getEventUuid)
import           RON.Types (Frame', Object (..), Op' (..), StateChunk (..),
                            UUID)
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

newLwwFrame
    :: (Clock clock, ReplicatedAsObject a)
    => [(UUID, I ReplicatedAsPayload)] -> clock (Object a)
newLwwFrame fields = collectFrame $ do
    payloads <- for fields $ \(_, I value) -> newPayload value
    e <- lift getEventUuid
    tell $ Map.singleton (lwwType, e) $ StateChunk e
        [Op' e name p | ((name, _), p) <- zip fields payloads]
    pure e

getLwwField
    :: ReplicatedAsPayload a
    => UUID -> StateChunk -> Frame' -> Either String a
getLwwField name StateChunk{..} frame = fmapL ("getLwwField:\n" <>) $ do
    let ops = filter ((name ==) . opRef) stateBody
    Op'{..} <- case ops of
        []   -> Left $ unwords ["no such name", show name, "in lww chunk", show stateBody, "in frame\n", show frame]
        [op] -> pure op
        _    -> Left "unreduced state"
    fromPayload opPayload frame

writeLwwField
    :: (Clock m, MonadError String m, MonadState (Object a) m)
    => UUID
    -> I ReplicatedAsPayload
    -> m ()
writeLwwField field (I value) = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    advanceToUuid stateVersion
    let chunk = filter ((field /=) . opRef) stateBody
    e <- getEventUuid
    (p, newFrame) <- runWriterT $ newPayload value
    let newOp = Op' e field p
    let chunk' = sortOn opRef $ newOp : chunk
    let state' = StateChunk e chunk'
    put Object
        { objectFrame = Map.insert objectId state' objectFrame <> newFrame
        , ..
        }
