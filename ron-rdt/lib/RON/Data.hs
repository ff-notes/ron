{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Typed and untyped RON tools
module RON.Data (
    Reducible (..),
    Rep,
    Replicated (..),
    execDocState,
    getObjectStateChunk,
    newDocFrameWith,
    newEmptyDocFrame,
    newObject,
    rconcat,
    reduceObject,
    reduceStateFrame,
    reduceWireFrame,
    stateFromWireChunk,
    stateToWireChunk,
    ) where

import           RON.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Data.LWW (LwwRep)
import           RON.Data.ORSet (ORSetRep)
import           RON.Data.RGA (RgaRep)
import           RON.Data.VersionVector (VersionVector)
import           RON.Error (MonadE, throwErrorString)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (ClosedOp (..), ObjectRef (ObjectRef), Op (..),
                            StateChunk (..), StateFrame, UUID,
                            WireChunk (Closed, Query, Value), WireFrame,
                            WireReducedChunk (..),
                            WireStateChunk (WireStateChunk, stateBody, stateType))
import           RON.UUID (pattern Zero)

reducers :: Map UUID Reducer
reducers = Map.fromList
    [ mkReducer @LwwRep
    , mkReducer @RgaRep
    , mkReducer @ORSetRep
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
    , Reducer
        {wireReducer = mkWireReducer @a, stateReducer = unsafeReduceState @a}
    )
  where
    -- | Reduce states using type @rep@ without checking 'stateType'
    unsafeReduceState
        :: forall rep
        . Reducible rep => WireStateChunk -> WireStateChunk -> WireStateChunk
    unsafeReduceState
            WireStateChunk{stateBody = c1} WireStateChunk{stateBody = c2} =
        wireStateChunk @rep $ reduceState (StateChunk c1) (StateChunk c2)

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
            reducedStateBody = stateToChunk @a reducedState
            rc = ReducedChunk{rcRef = Zero, rcBody = reducedStateBody}
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
                        ( [(opId $ op wrcHeader, stateFromChunk wrcBody)]
                        , []
                        , []
                        , []
                        )
                _ ->  -- patch
                    pure
                        ( []
                        , [ReducedChunk{rcRef = ref, rcBody = wrcBody}]
                        , []
                        , []
                        )
        _ -> Nothing
    guardSameObject ClosedOp{reducerId, objectId} =
        guard $ reducerId == typ && objectId == obj
    wrapRChunk ReducedChunk{rcRef, rcBody} = WireReducedChunk
        { wrcHeader = wrapOp Op{opId = Zero, refId = rcRef, payload = []}
        , wrcBody   = rcBody
        }

reduceStateFrame :: MonadE m => StateFrame -> StateFrame -> m StateFrame
reduceStateFrame s1 s2 =
    (`execStateT` s1) . (`Map.traverseWithKey` s2) $ \oid chunk -> let
        WireStateChunk{stateType} = chunk
        in
        case reducers !? stateType of
            Just Reducer{stateReducer} ->
                modify' $ Map.insertWith stateReducer oid chunk
            Nothing ->
                throwErrorString $
                "Cannot reduce StateFrame of unknown type " ++ show stateType

-- | Reduce object with frame from another version of the same object,
-- ignoring its id.
unsafeReduceObject
    :: MonadE m => DocFrame a -> StateFrame -> m (DocFrame a)
unsafeReduceObject obj@DocFrame{frame = s1} s2 = do
    frame' <- reduceStateFrame s1 s2
    pure obj{frame = frame'}

-- | Reduce object with frame from another version of the same object.
reduceObject :: MonadE m => DocFrame a -> DocFrame a -> m (DocFrame a)
reduceObject o1@DocFrame{uuid = id1} DocFrame{uuid = id2, frame = frame2}
    | id1 == id2 = unsafeReduceObject o1 frame2
    | otherwise  = throwErrorString $ "Object ids differ: " ++ show (id1, id2)

newtype MaxOnFst a b = MaxOnFst (a, b)

instance Ord a => Semigroup (MaxOnFst a b) where
    mof1@(MaxOnFst (a1, _)) <> mof2@(MaxOnFst (a2, _))
        | a1 < a2   = mof2
        | otherwise = mof1

-- execDocState_ :: Monad m => StateT StateFrame m a -> m StateFrame
-- execDocState_ action = execStateT action mempty

newEmptyDocFrame :: ReplicaClock m => m (DocFrame a)
newEmptyDocFrame = do
    uuid <- getEventUuid
    pure DocFrame{uuid, frame = mempty}

newDocFrameWith ::
    ReplicaClock m => (ObjectRef a -> StateT StateFrame m b) -> m (DocFrame a)
newDocFrameWith action = do
    uuid <- getEventUuid
    frame <- execStateT (action $ ObjectRef uuid) mempty
    pure DocFrame{uuid, frame}

newObject :: ReplicaClock m => m (ObjectRef a)
newObject = ObjectRef <$> getEventUuid

execDocState ::
    Monad m =>
    DocFrame a -> (ObjectRef a -> StateT StateFrame m b) -> m (DocFrame a)
execDocState DocFrame{uuid, frame} action = do
    frame' <- execStateT (action $ ObjectRef uuid) frame
    pure DocFrame{uuid, frame = frame'}
