{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Typed and untyped RON tools
module RON.Data (
    Reducible (..),
    Replicated (..),
    ReplicatedAsObject (..),
    ReplicatedAsPayload (..),
    fromRon,
    mkStateChunk,
    newRon,
    objectEncoding,
    payloadEncoding,
    reduceObject,
    reduceStateFrame,
    reduceWireFrame,
) where

import           RON.Internal.Prelude

import           Control.Monad.State.Strict (execStateT, lift, modify')
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
import           RON.Types (Object (..), Op (..), RawOp (..), StateChunk (..),
                            StateFrame, UUID, WireChunk (Query, Raw, Value),
                            WireFrame, WireReducedChunk (..))
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
    Nothing ->
        toList  -- TODO(2018-11-08, cblp, #27) use default reducer
    Just Reducer{wireReducer} -> wireReducer obj

isQuery :: WireChunk -> Bool
isQuery = \case
    Query _ -> True
    _       -> False

mkReducer :: forall a . Reducible a => (UUID, Reducer)
mkReducer =
    ( reducibleOpType @a
    , Reducer{wireReducer = mkWireReducer @a, stateReducer = reduceState @a}
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
            in
            (Just $ Value $ wrapRChunk rc, reduceUnappliedPatches @a unapplied')
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

reduceState :: forall a . Reducible a => StateChunk -> StateChunk -> StateChunk
reduceState s1 s2 =
    stateToChunk @a $ ((<>) `on` (stateFromChunk . stateBody)) s1 s2

reduceStateFrame :: StateFrame -> StateFrame -> Either String StateFrame
reduceStateFrame s1 s2 =
    (`execStateT` s1) . (`Map.traverseWithKey` s2) $ \oid@(typ, _) chunk ->
        case reducers !? typ of
            Just Reducer{stateReducer} ->
                modify' $ Map.insertWith stateReducer oid chunk
            Nothing -> lift $
                Left $ "Cannot reduce StateFrame of unknown type " ++ show typ

unsafeReduceObject :: Object a -> StateFrame -> Either String (Object a)
unsafeReduceObject Object{objectId, objectFrame = s1} s2 = do
    objectFrame <- reduceStateFrame s1 s2
    pure Object{..}

-- | Reduce object with frame from another version of the same object.
reduceObject :: Object a -> Object a -> Either String (Object a)
reduceObject o1 o2
    | id1 == id2 = unsafeReduceObject o1 $ objectFrame o2
    | otherwise  = Left $ "Object ids differ: " ++ show (id1, id2)
  where
    id1 = objectId o1
    id2 = objectId o2
