{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LWW (..)
    , lwwReduce
    , lwwType
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducer)
import           RON.Event (EpochEvent, decodeEvent, encodeEvent,
                            fromEpochEvent, getEvent, toEpochEvent)
import           RON.Typed (AsAtom, Replicated, View, fromAtom, fromStateChunk,
                            fromStateOps, initialize, toAtom, toStateChunk,
                            toStateOps, view)
import           RON.Types (Chunk (..), Op (..), ReducedChunk (..), UUID)
import qualified RON.UUID as UUID

data LWW a = LWW
    { time  :: !EpochEvent
    , value :: !a
    }
    deriving (Eq, Show)

-- | Merge by choosing more recent timestamp.
instance Semigroup (LWW a) where
    x <> y
        | time x < time y = y
        | otherwise       = x

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

instance AsAtom a => Replicated (LWW a) where
    type View (LWW a) = a
    view = value

    initialize value = do
        time <- getEvent
        pure LWW{time, value}

    toStateOps this lww = pure [toOp this lww]

    toStateChunk this lww =
        pure $ ReducedChunk{chunkHeader = toOp this lww, chunkBody = []}

    fromStateOps _ ownOps _ = case ownOps of
        []     -> Left "Empty state"
        op:ops -> fmap sconcat . traverse fromOp $ op :| ops

    fromStateChunk op _ _ = fromOp op

toOp :: (AsAtom a) => UUID -> LWW a -> Op
toOp this LWW{time, value} = Op
    { opType     = lwwType
    , opObject   = this
    , opEvent    = encodeEvent $ fromEpochEvent time
    , opLocation = UUID.zero
    , opPayload  = [toAtom value]
    }

fromOp :: AsAtom a => Op -> Either String (LWW a)
fromOp Op{opEvent, opPayload} = do
    time <- maybe (Left "Bad event") pure $ toEpochEvent event
    case opPayload of
        [a] -> do
            value <- maybe (Left "Bad atom") Right $ fromAtom a
            pure LWW{time, value}
        _   -> Left "Bad opPayload"
  where
    event = decodeEvent  opEvent

-- | Time is 'opEvent lpfOp'. Value is 'opPayload lpfOp', actually the whole op.
data LwwPerField = LwwPerField{lpfBaseEvent :: !UUID, lpfOp :: !Op}

instance Semigroup LwwPerField where
    x <> y
        | ((<) `on` (opEvent . lpfOp)) x y = y
        | otherwise                        = x

data LpfObject = LpfObject
    {loBaseEvent :: UUID, loFields :: Map UUID LwwPerField, loLeftovers :: [Op]}

instance Semigroup LpfObject where
    LpfObject base1 fields1 leftovers1 <> LpfObject base2 fields2 leftovers2 =
        LpfObject
            (min base1 base2)
            (Map.unionWith (<>) fields1 fields2)
            (leftovers1 <> leftovers2)

lwwReduce :: Reducer
lwwReduce obj chunks = maybeToList reduced ++ leftovers
  where
    reduced = case reduceables of
        []     -> Nothing
        r : rs -> Just $ toChunk obj (sconcat $ r :| rs)
    (leftovers, reduceables) = partitionEithers
        [maybe (Left chunk) Right $ fromChunk chunk | chunk <- chunks]

fromChunk :: Chunk -> Maybe LpfObject
fromChunk = \case
    Raw op@Op{opEvent, opLocation} -> Just LpfObject
        { loBaseEvent = opEvent
        , loFields    =
            Map.singleton
                opLocation
                LwwPerField{lpfBaseEvent = opEvent, lpfOp = op}
        , loLeftovers = []
        }
    Value ReducedChunk{chunkHeader, chunkBody} -> Just LpfObject
        { loBaseEvent = opLocation chunkHeader
        , loFields    = Map.fromListWith (<>) reduceables
        , loLeftovers
        }
      where
        (loLeftovers, reduceables) = partitionEithers
            [maybe (Left op) Right $ lpfFromOp op | op <- chunkBody]
        lpfFromOp op = do
            guard $ opType op == lwwType && opObject op == opObject chunkHeader
            pure
                ( opLocation op
                , LwwPerField{lpfBaseEvent = opLocation chunkHeader, lpfOp = op}
                )
    Query _ -> Nothing

toChunk :: UUID -> LpfObject -> Chunk
toChunk obj LpfObject{loBaseEvent, loFields, loLeftovers} = Value ReducedChunk
    { chunkHeader = Op
        { opType     = lwwType
        , opObject   = obj
        , opEvent    = chunkEvent
        , opLocation = chunkLocation
        , opPayload  = []
        }
    , chunkBody = map lpfOp (Map.elems loFields) ++ loLeftovers
    }
  where
    chunkEvent = maximumDef UUID.zero $ opEvent . lpfOp <$> loFields
    chunkLocation =
        minimum $ loBaseEvent : map lpfBaseEvent (Map.elems loFields)
