{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Data
    ( reduce
    ) where

import           Data.Foldable (fold)
import           Data.List (partition)
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducer)
import           RON.Data.LWW (lwwReduce, lwwType)
import           RON.Data.VersionVector (vvReduce, vvType)
import           RON.Types (Chunk (Query, Raw, Value), Frame, Op (Op),
                            ReducedChunk (ReducedChunk), UUID, chunkHeader,
                            opObject, opType)

reducers :: Map UUID Reducer
reducers = Map.fromList [(lwwType, lwwReduce), (vvType, vvReduce)]

reduce :: Frame -> Frame
reduce chunks = values' ++ queries
  where
    chunkObjectAndType = opObjectAndType . \case
        Raw                              op  -> op
        Value ReducedChunk{chunkHeader = op} -> op
        Query ReducedChunk{chunkHeader = op} -> op
    opObjectAndType Op{..} = (opObject, opType)
    (queries, values) = partition isQuery chunks
    values' =
        fold $
        Map.mapWithKey reduceByType $
        Map.fromListWith (++)
            [(chunkObjectAndType value, [value]) | value <- values]

reduceByType :: (UUID, UUID) -> [Chunk] -> [Chunk]
reduceByType (obj, typ) chunks = case reducers !? typ of
    Nothing      -> chunks
    Just reducer -> reducer obj chunks

isQuery :: Chunk -> Bool
isQuery = \case
    Query _ -> True
    _       -> False
