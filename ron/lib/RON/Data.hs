{-# LANGUAGE AllowAmbiguousTypes #-}
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
    , reduceWireFrame
    ) where

import           RON.Internal.Prelude

import           Data.Foldable (fold)
import           Data.List (partition)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Data.LWW (LwwPerField)
import           RON.Data.ORSet (ORSetRaw)
import           RON.Data.RGA (RgaRaw)
import           RON.Data.VersionVector (VersionVector)
import           RON.Types (Op (..), RawOp (..), StateChunk (..), UUID,
                            WireChunk (Query, Raw, Value), WireFrame,
                            WireReducedChunk (..))
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

reducers :: Map UUID Reducer
reducers = Map.fromList
    [ mkReducer @LwwPerField
    , mkReducer @RgaRaw
    , mkReducer @ORSetRaw
    , mkReducer @VersionVector
    ]

reduceWireFrame :: WireFrame -> WireFrame
reduceWireFrame chunks = values' ++ queries where
    chunkTypeAndObject = opTypeAndObject . \case
        Raw                                op  -> op
        Value WireReducedChunk{wrcHeader = op} -> op
        Query WireReducedChunk{wrcHeader = op} -> op
    opTypeAndObject RawOp{..} = (opType, opObject)
    (queries, values) = partition isQuery chunks
    values' =
        fold $
        Map.mapWithKey reduceWireFrameByType $
        NonEmpty.fromList <$>
        Map.fromListWith (++)
            [(chunkTypeAndObject value, [value]) | value <- values]

reduceWireFrameByType :: (UUID, UUID) -> NonEmpty WireChunk -> [WireChunk]
reduceWireFrameByType (typ, obj) = case reducers !? typ of
    Nothing                   -> toList  -- TODO use default reducer
    Just Reducer{wireReducer} -> wireReducer obj

isQuery :: WireChunk -> Bool
isQuery = \case
    Query _ -> True
    _       -> False

mkReducer :: forall a . Reducible a => (UUID, Reducer)
mkReducer =
    ( reducibleOpType @a
    , Reducer{wireReducer = mkWireReducer @a, stateReducer = mkStateReducer @a}
    )

mkWireReducer :: forall a . Reducible a => WireReducer
mkWireReducer obj chunks = chunks' <> leftovers where
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
                stateToChunk @a reducedState
            MaxOnFst (seenStateVersion, seenState) =
                sconcat $ fmap MaxOnFst nStates
            stateVersion = if
                | reducedStateVersion > seenStateVersion -> reducedStateVersion
                | reducedState == seenState -> seenStateVersion
                | otherwise -> UUID.succValue seenStateVersion
            rc = ReducedChunk
                { rcVersion = stateVersion
                , rcRef = Zero
                , rcBody = reducedStateBody
                }
            in  ( Just $ Value $ wrapRChunk rc
                , reduceUnappliedPatches @a unapplied'
                )
    typ = reducibleOpType @a
    wrapOp = RawOp typ obj
    (states, patches, rawops, leftovers) = foldMap load chunks
    load chunk = fromMaybe ([], [], [], [chunk]) $ load' chunk
    load' chunk = case chunk of
        Raw rawop@RawOp{op} -> do
            guardSameObject rawop
            pure ([], [], [op], [])
        Value WireReducedChunk{wrcHeader, wrcBody} -> do
            guardSameObject wrcHeader
            let ref = opRef $ op wrcHeader
            case ref of
                Zero ->  -- state
                    pure
                        ( [ ( opEvent $ op wrcHeader
                            , stateFromChunk wrcBody
                            ) ]
                        , []
                        , []
                        , []
                        )
                _ ->  -- patch
                    pure
                        ( []
                        ,   [ ReducedChunk
                                { rcVersion = opEvent $ op wrcHeader
                                , rcRef = ref
                                , rcBody = wrcBody
                                }
                            ]
                        , []
                        , []
                        )
        _ -> Nothing
    guardSameObject RawOp{opType, opObject} =
        guard $ opType == typ && opObject == obj
    wrapRChunk ReducedChunk{..} = WireReducedChunk
        { wrcHeader = wrapOp
            Op{opEvent = rcVersion, opRef = rcRef, opPayload = []}
        , wrcBody = rcBody
        }

mkStateReducer
    :: forall a . Reducible a => StateChunk -> StateChunk -> StateChunk
mkStateReducer c1 c2 =
    stateToChunk @a $ ((<>) `on` (stateFromChunk . stateBody)) c1 c2
