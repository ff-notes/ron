{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Data.ORSet
    ( AsORSet (..)
    , ORSet
    , add
    , removeBy
    , removeRef
    , removeValue
    ) where

import           RON.Internal.Prelude

import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (StateT, get, put)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import qualified Data.Map.Strict as Map
import           GHC.Exts (IsList, Item)
import qualified GHC.Exts as Exts

import           RON.Data.Internal
import           RON.Event (Clock, getEventUuid)
import           RON.Types (Atom, Object (..), Op' (..), StateChunk (..), UUID)
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

newtype AsORSet set = AsORSet set

instance (IsList set, Replicated (Item set)) => Replicated (AsORSet set) where
    encoding = objectEncoding

instance (IsList set, Replicated (Item set)) => ReplicatedAsObject (AsORSet set)
    where

    objectOpType = setType

    newObject (AsORSet items) = collectFrame $ do
        ops <- for (Exts.toList items) $ \a -> do
            e <- lift getEventUuid
            payload <- newRon a
            pure $ Op' e zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (setType, oid) $ StateChunk version ops
        pure oid

    getObject obj@Object{..} = do
        StateChunk{..} <- getObjectStateChunk obj
        items <- for stateBody $ \Op'{..} -> do
            value <- fromRon opPayload objectFrame
            pure (opRef, value)
        pure $ AsORSet $
            Exts.fromList [value | (opRef, value) <- items, opRef == zero]

add :: (IsList set, Replicated (Item set), Clock m, MonadError String m)
    => Item set -> StateT (Object (AsORSet set)) m ()
add value = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    e <- getEventUuid
    (p, newFrame) <- runWriterT $ newRon value
    let newOp = Op' e zero p
    let chunk' = stateBody ++ [newOp]
    let state' = StateChunk e chunk'
    put Object
        { objectFrame =
            Map.insert (setType, objectId) state' objectFrame
            <> newFrame
        , ..
        }

removeBy :: ([Atom] -> Bool) -> StateT (Object (AsORSet set)) m ()
removeBy = undefined

removeValue
    :: ReplicatedAsPayload (Item set)
    => Item set -> StateT (Object (AsORSet set)) m ()
removeValue = removeBy . eqPayload

-- TODO Item set ~ Object a =>
removeRef :: Object a -> StateT (Object (AsORSet set)) m ()
removeRef = removeBy . eqRef
