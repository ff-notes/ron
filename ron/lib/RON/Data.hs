{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Data
    ( Reducible (..)
    , Replicated (..)
    , ReplicatedAsObject (..)
    , ReplicatedAsPayload (..)
    , collectFrame
    , getObjectStateChunk
    , objectEncoding
    , payloadEncoding
    , reduce
    , typeName
    ) where

import           RON.Internal.Prelude

import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (fold)
import           Data.List (partition)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           GHC.TypeLits (symbolVal)

import           RON.Data.Internal
import           RON.Data.LWW (LwwPerField)
import           RON.Data.ORSet (ORSet)
import           RON.Data.RGA (RgaRaw)
import           RON.Data.VersionVector (VersionVector)
import           RON.Types (Chunk (Query, Raw, Value), Frame, Op (..), Op' (..),
                            RChunk (..), StateChunk (..), UUID)
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

reducers :: Map UUID Reducer
reducers = Map.fromList
    [ mkReducer @LwwPerField
    , mkReducer @RgaRaw
    , mkReducer @ORSet
    , mkReducer @VersionVector
    ]

reduce :: Frame -> Frame
reduce chunks = values' ++ queries where
    chunkObjectAndType = opObjectAndType . \case
        Raw                         op  -> op
        Value RChunk{rchunkHeader = op} -> op
        Query RChunk{rchunkHeader = op} -> op
    opObjectAndType Op{..} = (opObject, opType)
    (queries, values) = partition isQuery chunks
    values' =
        fold $
        Map.mapWithKey reduceByType $
        NonEmpty.fromList <$>
        Map.fromListWith (++)
            [(chunkObjectAndType value, [value]) | value <- values]

reduceByType :: (UUID, UUID) -> NonEmpty Chunk -> [Chunk]
reduceByType (obj, typ) = case reducers !? typ of
    Nothing   -> toList  -- TODO use generic reducer
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
reducer obj chunks = chunks' ++ leftovers where
    chunks'
        =  maybeToList stateChunk'
        ++ map (Value . wrapRChunk) unappliedPatches
        ++ map (Raw . wrapOp) unappliedOps
    mStates = nonEmpty states
    (stateChunk', (unappliedPatches, unappliedOps)) = case mStates of
        Nothing -> (Nothing, reduceUnappliedPatches @a (patches, rawops))
        Just nStates -> let
            state = sconcat $ fmap snd nStates
            (reducedState, unapplied') = applyPatches state (patches, rawops)
            StateChunk reducedStateVersion reducedStateBody =
                stateToChunk reducedState
            MaxOnFst (seenStateVersion, seenState) =
                sconcat $ fmap MaxOnFst nStates
            stateVersion = if
                | reducedStateVersion > seenStateVersion -> reducedStateVersion
                | reducedState == seenState -> seenStateVersion
                | otherwise -> UUID.succValue seenStateVersion
            rc = RChunk'
                { rchunk'Version = stateVersion
                , rchunk'Ref = Zero
                , rchunk'Body = reducedStateBody
                }
            in  ( Just $ Value $ wrapRChunk rc
                , reduceUnappliedPatches @a unapplied'
                )
    typ = typeName @a
    wrapOp = Op typ obj
    (states :: [(UUID, a)], patches :: [RChunk'], rawops :: [Op'], leftovers :: [Chunk])
        = foldMap load chunks
    load chunk = fromMaybe ([], [], [], [chunk]) $ load' chunk
    load' chunk = case chunk of
        Raw op@Op{op'} -> do
            guardSameObject op
            pure ([], [], [op'], [])
        Value RChunk{rchunkHeader, rchunkBody} -> do
            guardSameObject rchunkHeader
            body <- for rchunkBody $ \op -> do
                guardSameObject op
                pure $ op' op
            let ref = opRef $ op' rchunkHeader
            case ref of
                Zero ->  -- state
                    -- let state = fromChunk @a ref body
                    pure
                        ( [(opEvent $ op' rchunkHeader, stateFromChunk body)]
                        , []
                        , []
                        , []
                        )
                _ ->  -- patch
                    pure
                        ( []
                        ,   [ RChunk'
                                { rchunk'Version = opEvent $ op' rchunkHeader
                                , rchunk'Ref = ref
                                , rchunk'Body = body
                                }
                            ]
                        , []
                        , []
                        )
        _ -> Nothing
    guardSameObject Op{opType, opObject} =
        guard $ opType == typ && opObject == obj
    wrapRChunk RChunk'{..} = RChunk
        { rchunkHeader = wrapOp
            Op'{opEvent = rchunk'Version, opRef = rchunk'Ref, opPayload = []}
        , rchunkBody = map wrapOp rchunk'Body
        }
