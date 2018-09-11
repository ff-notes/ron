{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet (ORSet, ORSetHash (..), add) where

import           RON.Internal.Prelude

import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (StateT, get, put)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), ReplicatedAsObject (..),
                                    ReplicatedAsPayload (..), collectFrame,
                                    getObjectStateChunk, mkStateChunk)
import           RON.Event (Clock, getEventUuid)
import           RON.Types (Object (..), Op' (..), StateChunk (..), UUID)
import           RON.UUID (zero)
import qualified RON.UUID as UUID

data SetItem = SetItem{itemIsAlive :: Bool, itemOriginalOp :: Op'}
    deriving (Eq, Show)

instance Semigroup SetItem where
    (<>) = minOn itemIsAlive

itemFromOp :: Op' -> (UUID, SetItem)
itemFromOp itemOriginalOp@Op'{..} = (itemId, item) where
    itemIsAlive = opRef == zero
    itemId = if itemIsAlive then opEvent else opRef
    item = SetItem{..}

newtype ORSet = ORSet (Map UUID SetItem)
    deriving (Eq, Show)

instance Semigroup ORSet where
    ORSet set1 <> ORSet set2 = ORSet $ Map.unionWith (<>) set1 set2

instance Monoid ORSet where
    mempty = ORSet mempty

instance Reducible ORSet where
    type OpType ORSet = "set"

    stateFromChunk = ORSet . Map.fromListWith (<>) . map itemFromOp

    stateToChunk (ORSet set) =
        mkStateChunk . sortOn opEvent . map itemOriginalOp $ Map.elems set

setType :: UUID
setType = fromJust $ UUID.mkName "set"

newtype ORSetHash a = ORSetHash (HashSet a)

instance (Eq a, Hashable a, ReplicatedAsPayload a)
    => ReplicatedAsPayload (ORSetHash a)

instance (Eq a, Hashable a, ReplicatedAsPayload a)
    => ReplicatedAsObject (ORSetHash a) where

    objectOpType = setType

    newObject (ORSetHash items) = collectFrame $ do
        ops <- for (toList items) $ \a -> do
            e <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' e zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (setType, oid) $ StateChunk version ops
        pure oid

    getObject obj@Object{..} = do
        StateChunk{..} <- getObjectStateChunk obj
        items <- for stateBody $ \Op'{..} -> do
            value <- fromPayload opPayload objectFrame
            pure (opRef, value)
        pure $ ORSetHash $ HashSet.fromList
            [value | (opRef, value) <- items, opRef == zero]

add :: (ReplicatedAsPayload a, Clock m, MonadError String m)
    => a -> StateT (Object (ORSetHash a)) m ()
add value = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    e <- getEventUuid
    (p, newFrame) <- runWriterT $ newPayload value
    let newOp = Op' e zero p
    let chunk' = stateBody ++ [newOp]
    let state' = StateChunk e chunk'
    put Object
        { objectFrame = Map.insert objectId state' objectFrame <> newFrame
        , ..
        }
