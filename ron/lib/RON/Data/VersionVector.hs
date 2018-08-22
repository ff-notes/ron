{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Data.VersionVector
    ( vvReduce
    , vvType
    ) where

import           Control.Monad (guard)
import           Data.Either (partitionEithers)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, maybeToList)
import           Data.Semigroup (Semigroup, sconcat)

import           RON.Data.Internal (Reducer)
import           RON.Internal.Word (Word64)
import           RON.Types (Chunk (Query, Raw, Value), Op (Op),
                            ReducedChunk (ReducedChunk), UUID (UUID), chunkBody,
                            chunkHeader, opEvent, opLocation, opObject,
                            opPayload, opType)
import qualified RON.UUID as UUID

type Origin = Word64

type Time = Word64

data VersionVector = VersionVector
    {vvBaseEvent :: UUID, vvVersions :: Map Origin Time, vvLeftovers :: [Op]}

instance Semigroup VersionVector where
    VersionVector base1 vers1 left1 <> VersionVector base2 vers2 left2 =
        VersionVector
            (min base1 base2) (Map.unionWith max vers1 vers2) (left1 ++ left2)

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
    Raw Op{opEvent} -> Just VersionVector
        { vvBaseEvent = opEvent
        , vvVersions  = Map.singleton origin time
        , vvLeftovers = []
        }
      where
        UUID time origin = opEvent
    Value ReducedChunk{chunkHeader, chunkBody} -> Just VersionVector
        { vvBaseEvent = opLocation chunkHeader
        , vvVersions  = Map.fromListWith max reduceables
        , vvLeftovers
        }
      where
        (vvLeftovers, reduceables) = partitionEithers
            [maybe (Left op) Right $ fromOp op | op <- chunkBody]
        fromOp op = do
            guard $ opType op == vvType && opObject op == opObject chunkHeader
            pure (origin, time)
          where
            UUID time origin = opEvent op
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
        , chunkBody = map vvToOp (Map.assocs vvVersions) ++ vvLeftovers
        }
  where
    chunkEvent
        | null events = UUID.zero
        | otherwise   = maximum events
    chunkLocation = minimum $ vvBaseEvent : events
    vvToOp (origin, time) = Op
        { opType     = vvType
        , opObject   = obj
        , opEvent    = UUID time origin
        , opLocation = UUID.zero
        , opPayload  = []
        }
    events = [UUID time origin | (origin, time) <- Map.assocs vvVersions]
