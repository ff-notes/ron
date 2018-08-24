{-# LANGUAGE DataKinds #-}
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

import           RON.Data.Internal (OpType, Reducer, Reducible, applyOp,
                                    initial, toStateChunk)
import           RON.Event (EpochEvent, decodeEvent, encodeEvent,
                            fromEpochEvent, getEvent, toEpochEvent)
import           RON.Typed (AsAtom, Replicated, View, fromAtom, fromStateChunk,
                            fromStateOps, initialize, toAtom, toStateChunk,
                            toStateOps, view)
import           RON.Types (Chunk (Query, Raw, Value), Op (Op), RChunk (RChunk),
                            ROp (ROp), UUID, chunkBody, chunkHeader, opLocation,
                            opObject, opR, opType, ropEvent, ropLocation,
                            ropPayload)
import qualified RON.UUID as UUID

--------------------------------------------------------------------------------
-- Typed -----------------------------------------------------------------------
--------------------------------------------------------------------------------

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

    toStateOps this s = pure [toOp this s]

    toStateChunk this s =
        pure $ RChunk{chunkHeader = toOp this s, chunkBody = []}

    fromStateOps _ ownOps _ = case ownOps of
        []     -> Left "Empty state"
        op:ops -> fmap sconcat . traverse fromOp $ op :| ops

    fromStateChunk op _ _ = fromOp op

toOp :: (AsAtom a) => UUID -> LWW a -> Op
toOp this LWW{time, value} = Op
    { opType   = lwwType
    , opObject = this
    , opR      = ROp
        { ropEvent    = encodeEvent $ fromEpochEvent time
        , ropLocation = UUID.zero
        , ropPayload  = [toAtom value]
        }
    }

fromOp :: AsAtom a => Op -> Either String (LWW a)
fromOp Op{opR = ROp{ropEvent, ropPayload}} = do
    time <- maybe (Left "Bad event") pure $ toEpochEvent event
    case ropPayload of
        [a] -> do
            value <- maybe (Left "Bad atom") Right $ fromAtom a
            pure LWW{time, value}
        _   -> Left "Bad payload"
  where
    event = decodeEvent ropEvent

--------------------------------------------------------------------------------
-- Just function ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Time is 'opEvent lpfOp'. Value is 'opPayload lpfOp', actually the whole op.
data LPF = LPF{lpfBaseEvent :: !UUID, lpfOp :: !Op}

instance Semigroup LPF where
    x <> y
        | ((<) `on` (ropEvent . opR . lpfOp)) x y = y
        | otherwise                        = x

data LpfObject = LpfObject
    {loBaseEvent :: UUID, loFields :: Map UUID LPF, loLeftovers :: [Op]}

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
    Raw op@Op{opR = ROp{ropEvent, ropLocation}} -> Just LpfObject
        { loBaseEvent = ropEvent
        , loFields    =
            Map.singleton
                ropLocation
                LPF{lpfBaseEvent = ropEvent, lpfOp = op}
        , loLeftovers = []
        }
    Value RChunk{chunkHeader, chunkBody} -> Just LpfObject
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
                , LPF{lpfBaseEvent = opLocation chunkHeader, lpfOp = op}
                )
    Query _ -> Nothing

toChunk :: UUID -> LpfObject -> Chunk
toChunk obj LpfObject{loBaseEvent, loFields, loLeftovers} = Value RChunk
    { chunkHeader = Op
        { opType     = lwwType
        , opObject   = obj
        , opR        = ROp
            { ropEvent    = chunkEvent
            , ropLocation = chunkLocation
            , ropPayload  = []
            }
        }
    , chunkBody = map lpfOp (Map.elems loFields) ++ loLeftovers
    }
  where
    chunkEvent = maximumDef UUID.zero $ ropEvent . opR . lpfOp <$> loFields
    chunkLocation =
        minimum $ loBaseEvent : map lpfBaseEvent (Map.elems loFields)

--------------------------------------------------------------------------------
-- Several functions -----------------------------------------------------------
--------------------------------------------------------------------------------

lww :: ROp -> ROp -> ROp
lww = maxOn ropEvent

newtype LwwPerField = LwwPerField (Map UUID ROp)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    type OpType LwwPerField = "lww"

    initial = LwwPerField mempty

    toStateChunk (LwwPerField fields) = Map.elems fields

    applyOp rop@ROp{ropLocation} (LwwPerField fields) =
        pure $ LwwPerField $ Map.insertWith lww ropLocation rop fields
