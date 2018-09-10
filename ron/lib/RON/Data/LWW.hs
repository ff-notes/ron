{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField (..)
    , getLwwField
    , lwwType
    , newLwwFrame
    ) where

import           RON.Internal.Prelude

import           Control.Monad.Writer.Strict (lift, tell)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), ReplicatedAsObject,
                                    ReplicatedAsPayload (..), collectFrame,
                                    mkStateChunk)
import           RON.Event (Clock, getEventUuid)
import           RON.Types (Object (..), Op' (..), StateChunk (..), UUID)
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
    :: ReplicatedAsPayload b
    => UUID -> StateChunk -> Object a -> Either String b
getLwwField name StateChunk{..} Object{..} = do
    let ops = filter ((name ==) . opRef) stateBody
    Op'{..} <- case ops of
        []   -> Left "no such name in lww chunk"
        [op] -> pure op
        _    -> Left "unreduced state"
    fromPayload opPayload objectFrame
