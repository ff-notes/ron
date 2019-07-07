{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
    getObjectStateChunk,
    mkStateChunk,
    newRon,
    objectEncoding,
    payloadEncoding,
    reduceObject,
    reduceStateFrame,
    reduceWireFrame,
    -- * 'ObjectState' monad
    evalObjectState,
    evalObjectState_,
    execObjectState,
    execObjectState_,
    newObjectState,
    newObjectStateWith,
    runObjectState,
    runObjectState_,
) where

import           RON.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Data.LWW (LwwPerField)
import           RON.Data.ORSet (ORSetRaw)
import           RON.Data.RGA (RgaRaw)
import           RON.Data.VersionVector (VersionVector)
import           RON.Error (MonadE, throwErrorString)
import           RON.Types (ClosedOp (..), Object (Object),
                            ObjectState (ObjectState), Op (..), StateChunk (..),
                            StateFrame, UUID, WireChunk (Closed, Query, Value),
                            WireFrame, WireReducedChunk (..), frame, id)
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
        Closed                             op  -> op
        Value WireReducedChunk{wrcHeader = op} -> op
        Query WireReducedChunk{wrcHeader = op} -> op
    opTypeAndObject ClosedOp{..} = (reducerId, objectId)
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
        ++ map (Closed . wrapOp) unappliedOps
    mStates = nonEmpty states
    (stateChunk', (unappliedPatches, unappliedOps)) = case mStates of
        Nothing -> (Nothing, reduceUnappliedPatches @a (patches, closedOps))
        Just nStates -> let
            nState = sconcat $ fmap snd nStates
            (reducedState, unapplied') =
                applyPatches nState (patches, closedOps)
            StateChunk
                    { stateVersion = reducedStateVersion
                    , stateBody = reducedStateBody
                    } =
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
    wrapOp = ClosedOp typ obj
    (states, patches, closedOps, leftovers) = foldMap load chunks
    load chunk = fromMaybe ([], [], [], [chunk]) $ load' chunk
    load' chunk = case chunk of
        Closed closedOp@ClosedOp{op} -> do
            guardSameObject closedOp
            pure ([], [], [op], [])
        Value WireReducedChunk{wrcHeader, wrcBody} -> do
            guardSameObject wrcHeader
            let ref = refId $ op wrcHeader
            case ref of
                Zero ->  -- state
                    pure
                        ( [ ( opId $ op wrcHeader
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
                                { rcVersion = opId $ op wrcHeader
                                , rcRef = ref
                                , rcBody = wrcBody
                                }
                            ]
                        , []
                        , []
                        )
        _ -> Nothing
    guardSameObject ClosedOp{reducerId, objectId} =
        guard $ reducerId == typ && objectId == obj
    wrapRChunk ReducedChunk{..} = WireReducedChunk
        { wrcHeader = wrapOp Op{opId = rcVersion, refId = rcRef, payload = []}
        , wrcBody   = rcBody
        }

reduceState :: forall a . Reducible a => StateChunk -> StateChunk -> StateChunk
reduceState s1 s2 =
    stateToChunk @a $ ((<>) `on` (stateFromChunk . stateBody)) s1 s2

reduceStateFrame :: MonadE m => StateFrame -> StateFrame -> m StateFrame
reduceStateFrame s1 s2 =
    (`execStateT` s1) . (`Map.traverseWithKey` s2) $ \oid chunk -> let
        StateChunk{stateType} = chunk
        in
        case reducers !? stateType of
            Just Reducer{stateReducer} ->
                modify' $ Map.insertWith stateReducer oid chunk
            Nothing ->
                throwErrorString $
                "Cannot reduce StateFrame of unknown type " ++ show stateType

unsafeReduceObject
    :: MonadE m => ObjectState a -> StateFrame -> m (ObjectState a)
unsafeReduceObject obj@ObjectState{frame = s1} s2 = do
    frame' <- reduceStateFrame s1 s2
    pure obj{frame = frame'}

-- | Reduce object with frame from another version of the same object.
reduceObject :: MonadE m => ObjectState a -> ObjectState a -> m (ObjectState a)
reduceObject o1@ObjectState{id = id1} ObjectState{id = id2, frame = frame2}
    | id1 == id2 = unsafeReduceObject o1 frame2
    | otherwise  = throwErrorString $ "Object ids differ: " ++ show (id1, id2)

newtype MaxOnFst a b = MaxOnFst (a, b)

instance Ord a => Semigroup (MaxOnFst a b) where
    mof1@(MaxOnFst (a1, _)) <> mof2@(MaxOnFst (a2, _))
        | a1 < a2   = mof2
        | otherwise = mof1

-- | Run ObjectState action
evalObjectState
    :: Monad m => ObjectState b -> (Object b -> StateT StateFrame m a) -> m a
evalObjectState ObjectState{id, frame} action =
    evalStateT (action $ Object id) frame

-- | Run ObjectState action, starting with an empty frame
evalObjectState_ :: Monad m => StateT StateFrame m a -> m a
evalObjectState_ action = evalStateT action mempty

-- | Run ObjectState action
execObjectState
    :: Monad m
    => ObjectState b -> (Object b -> StateT StateFrame m a) -> m (ObjectState b)
execObjectState ObjectState{id, frame} action = do
    frame' <- execStateT (action $ Object id) frame
    pure ObjectState{id, frame = frame'}

-- | Run ObjectState action, starting with an empty frame
execObjectState_ :: Monad m => StateT StateFrame m a -> m StateFrame
execObjectState_ action = execStateT action mempty

-- | Run ObjectState action
runObjectState
    :: Functor m
    => ObjectState b
    -> (Object b -> StateT StateFrame m a)
    -> m (a, ObjectState b)
runObjectState ObjectState{id, frame} action =
    runStateT (action $ Object id) frame
    <&> \(a, frame') -> (a, ObjectState{id, frame = frame'})

-- | Run ObjectState action, starting with an empty frame
runObjectState_ :: StateT StateFrame m a -> m (a, StateFrame)
runObjectState_ action = runStateT action mempty

-- | Create new 'ObjectState' with an action
newObjectStateWith
    :: Functor m => StateT StateFrame m (Object a) -> m (ObjectState a)
newObjectStateWith action =
    runObjectState_ action <&> \(Object id, frame) -> ObjectState{id, frame}
