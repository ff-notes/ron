{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.VersionVector
    ( vvReduce
    , vvType
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (OpType, Reducer, Reducible, applyOp,
                                    initial, toStateChunk)
import           RON.Types (Chunk (Query, Raw, Value), Op (Op), RChunk (RChunk),
                            ROp (ROp), UUID (UUID), chunkBody, chunkHeader,
                            opEvent, opLocation, opObject, opPayload, opType,
                            ropEvent)
import qualified RON.UUID as UUID

type Origin = Word64

data VV = VV
    {vvBaseEvent :: UUID, vvVersions :: Map Origin Op, vvLeftovers :: [Op]}

instance Semigroup VV where
    VV base1 vers1 left1 <> VV base2 vers2 left2 =
        VV
            (min base1 base2)
            (Map.unionWith (maxOn opTime) vers1 vers2)
            (left1 ++ left2)

vvType :: UUID
vvType = fromJust $ UUID.mkName "vv"

vvReduce :: Reducer
vvReduce obj chunks = maybeToList reduced ++ leftovers
  where
    reduced = case reduceables of
        []     -> Nothing
        r : rs -> Just $ toChunk obj (sconcat $ r :| rs)
    (leftovers, reduceables) = partitionEithers
        [maybe (Left chunk) Right $ fromChunk chunk | chunk <- chunks]

fromChunk :: Chunk -> Maybe VV
fromChunk = \case
    Raw op@Op{opEvent} -> Just VV
        { vvBaseEvent = opEvent
        , vvVersions  = Map.singleton origin op
        , vvLeftovers = []
        }
      where
        UUID _ origin = opEvent
    Value RChunk{chunkHeader, chunkBody} -> Just VV
        { vvBaseEvent = opLocation chunkHeader
        , vvVersions  = Map.fromListWith (maxOn opTime) reduceables
        , vvLeftovers
        }
      where
        (vvLeftovers, reduceables) = partitionEithers
            [maybe (Left op) Right $ fromOp op | op <- chunkBody]
        fromOp op = do
            guard $ opType op == vvType && opObject op == opObject chunkHeader
            pure (origin, op)
          where
            UUID _ origin = opEvent op
    Query _ -> Nothing

toChunk :: UUID -> VV -> Chunk
toChunk obj VV{vvBaseEvent, vvVersions, vvLeftovers} = Value
    RChunk
        { chunkHeader = Op
            { opType     = vvType
            , opObject   = obj
            , opEvent    = chunkEvent
            , opLocation = chunkLocation
            , opPayload  = []
            }
        , chunkBody = Map.elems vvVersions ++ vvLeftovers
        }
  where
    chunkEvent = maximumDef UUID.zero events
    chunkLocation = minimum $ vvBaseEvent : events
    events = [UUID (opTime op) origin | (origin, op) <- Map.assocs vvVersions]

opTime :: Op -> Word64
opTime Op{opEvent = UUID time _} = time

--------------------------------------------------------------------------------

ropTime :: ROp -> Word64
ropTime ROp{ropEvent = UUID time _} = time

ropOrigin :: ROp -> Word64
ropOrigin ROp{ropEvent = UUID _ origin} = origin

latter :: ROp -> ROp -> ROp
latter = maxOn ropTime

newtype VersionVector = VersionVector (Map Origin ROp)

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Reducible VersionVector where
    type OpType VersionVector = "vv"

    initial = VersionVector mempty

    applyOp rop (VersionVector vv) =
        pure $ VersionVector $ Map.insert (ropOrigin rop) rop vv

    toStateChunk (VersionVector vv) = Map.elems vv
