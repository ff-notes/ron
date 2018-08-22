{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Data.VersionVector
    ( vvReduce
    , vvType
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducer)
import           RON.Types (Chunk (Query, Raw, Value), Op (Op),
                            ReducedChunk (ReducedChunk), UUID (UUID), chunkBody,
                            chunkHeader, opEvent, opLocation, opObject,
                            opPayload, opType)
import qualified RON.UUID as UUID

type Origin = Word64

data VersionVector = VersionVector
    {vvBaseEvent :: UUID, vvVersions :: Map Origin Op, vvLeftovers :: [Op]}

instance Semigroup VersionVector where
    VersionVector base1 vers1 left1 <> VersionVector base2 vers2 left2 =
        VersionVector
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

fromChunk :: Chunk -> Maybe VersionVector
fromChunk = \case
    Raw op@Op{opEvent} -> Just VersionVector
        { vvBaseEvent = opEvent
        , vvVersions  = Map.singleton origin op
        , vvLeftovers = []
        }
      where
        UUID _ origin = opEvent
    Value ReducedChunk{chunkHeader, chunkBody} -> Just VersionVector
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

toChunk :: UUID -> VersionVector -> Chunk
toChunk obj VersionVector{vvBaseEvent, vvVersions, vvLeftovers} = Value
    ReducedChunk
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
