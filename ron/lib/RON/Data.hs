{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Data
    ( reduce
    ) where

import           RON.Internal.Prelude

import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (fold)
import           Data.List (partition)
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Min (Min))
import           GHC.TypeLits (symbolVal)

import           RON.Data.Internal (OpType, Reducer, Reducible, applyPatch,
                                    patchFromChunk, patchFromOp, patchToChunk,
                                    stateFromChunk, stateToChunk)
import           RON.Data.LWW (LwwPerField)
import           RON.Data.VersionVector (VersionVector)
import           RON.Types (Chunk (Query, Raw, Value), Frame, Op (Op),
                            RChunk (RChunk), ROp (ROp), UUID, chunkBody,
                            chunkHeader, opLocation, opObject, opR, opType,
                            ropEvent, ropLocation, ropPayload)
import qualified RON.UUID as UUID

reducers :: Map UUID Reducer
reducers = Map.fromList [mkReducer @LwwPerField, mkReducer @VersionVector]

reduce :: Frame -> Frame
reduce chunks = values' ++ queries where
    chunkObjectAndType = opObjectAndType . \case
        Raw                              op  -> op
        Value RChunk{chunkHeader = op} -> op
        Query RChunk{chunkHeader = op} -> op
    opObjectAndType Op{..} = (opObject, opType)
    (queries, values) = partition isQuery chunks
    values' =
        fold $
        Map.mapWithKey reduceByType $
        Map.fromListWith (++)
            [(chunkObjectAndType value, [value]) | value <- values]

reduceByType :: (UUID, UUID) -> [Chunk] -> [Chunk]
reduceByType (obj, typ) = case reducers !? typ of
    Nothing   -> id  -- TODO use generic reducer
    Just rdcr -> rdcr obj

isQuery :: Chunk -> Bool
isQuery = \case
    Query _ -> True
    _       -> False

typeName :: forall a . Reducible a => UUID
typeName =
    fromJust . UUID.mkName . BSC.pack $ symbolVal (Proxy :: Proxy (OpType a))

mkReducer :: forall a . Reducible a => (UUID, Reducer)
mkReducer = (typeName @a, reducer @a)

reducer :: forall a . Reducible a => Reducer
reducer obj chunks = chunk' ++ rLeftovers where
    chunk' = case (rState, rPatch) of
        (Nothing,    Nothing        ) -> []
        (Just state, Nothing        ) -> mkStateChunk state
        (Nothing,    Just patch     ) -> mkPatchChunk patch
        (Just state, Just (patch, _)) -> mkStateChunk $ applyPatch state patch
    mkStateChunk state = mkRChunk (ropsEvent rops) UUID.Zero rops
        where rops = stateToChunk state
    mkPatchChunk (patch, Min patchRef) = mkRChunk (ropsEvent rops) patchRef rops
        where rops = patchToChunk @a patch
    mkRChunk ropEvent ropLocation rops =
        [ Value RChunk
            { chunkHeader = mkOp ROp{ropEvent, ropLocation, ropPayload = []}
            , chunkBody   = map mkOp rops
            }
        ]
    typ = typeName @a
    mkOp = Op typ obj
    (rState, rPatch, !rLeftovers) = foldMap fromChunk chunks
    fromChunk chunk = fromMaybe (Nothing, Nothing, [chunk]) $ fromChunk' chunk
    fromChunk' chunk = case chunk of
        Raw op@Op{opR = rop@ROp{ropEvent}} -> do
            guardSameObject op
            pure (Nothing, Just (patchFromOp @a rop, Min ropEvent), [])
        Value RChunk{chunkHeader, chunkBody} -> do
            guardSameObject chunkHeader
            body <- for chunkBody $ \op -> do
                guardSameObject op
                pure $ opR op
            case opLocation chunkHeader of
                UUID.Zero -> pure (Just $ stateFromChunk @a body, Nothing, [])
                ref ->
                    pure (Nothing, Just (patchFromChunk @a body, Min ref), [])
        _ -> Nothing
    guardSameObject Op{opType, opObject} =
        guard $ opType == typ && opObject == obj
    ropsEvent = maximumDef UUID.Zero . map ropEvent
