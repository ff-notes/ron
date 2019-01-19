{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Replicated Growable Array (RGA)
module RON.Data.RGA
    ( RGA (..)
    , RgaRaw
    , RgaString
    , edit
    , editText
    , getList
    , getText
    , newFromList
    , newFromText
    , rgaType
    ) where

import           Data.Algorithm.Diff (Diff (Both, First, Second),
                                      getGroupedDiffBy)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Text as Text

import           RON.Data.Internal
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid,
                            getEventUuids)
import           RON.Types (Object (..), Op (..), StateChunk (..), UUID)
import           RON.Util.Word (pattern B11, ls60)
import           RON.UUID (pattern Zero, uuidVersion)
import qualified RON.UUID as UUID

-- | opEvent = vertex id
--   opRef:
--      0 = value is alive,
--      _ = tombstone event, value is backup for undo
--   opPayload: the value
type Vertex = Op

data VertexListItem = VertexListItem
    { itemValue :: Vertex
    , itemNext  :: Maybe UUID
    }
    deriving (Eq, Show)

data VertexList = VertexList
    { listHead  :: UUID
    , listItems :: HashMap UUID VertexListItem
    }
    deriving (Eq, Show)

instance Semigroup VertexList where
    (<>) = merge

vertexListToOps :: Maybe VertexList -> [Vertex]
vertexListToOps mv = case mv of
    Nothing -> []
    Just VertexList{..} -> go listHead listItems
  where
    go root items = let
        VertexListItem{..} =
            HashMap.lookupDefault
                (error $ unlines
                    $  ["Cannot find vertex id", show root, "in array"]
                    ++ map show (HashMap.toList items)
                    ++ ["Original array is", show $ fromJust mv])
                root
                items
        rest = case itemNext of
            Just next -> go next (HashMap.delete root items)
            Nothing -> []
        in itemValue : rest

vertexListFromOps :: [Vertex] -> Maybe VertexList
vertexListFromOps = foldr go mempty where
    go v@Op{opEvent = vid} vlist =
        Just $ VertexList{listHead = vid, listItems = vlist'}
      where
        item itemNext = VertexListItem{itemValue = v, itemNext}
        vlist' = case vlist of
            Nothing -> HashMap.singleton vid (item Nothing)
            Just VertexList{listHead, listItems} ->
                HashMap.insert vid (item $ Just listHead) listItems

-- | Untyped RGA
newtype RgaRaw = RgaRaw (Maybe VertexList)
    deriving (Eq, Monoid, Semigroup, Show)

data PatchSet = PatchSet
    { psPatches  :: Map UUID VertexList
        -- ^ the key is the parent event, the value is a non-empty VertexList
    , psRemovals :: Map UUID UUID
        -- ^ the key is the target event, the value is the tombstone event
    }
    deriving (Eq, Show)

instance Semigroup PatchSet where
    rga1 <> rga2 = reapplyPatchSet $ preMerge rga1 rga2

preMerge :: PatchSet -> PatchSet -> PatchSet
preMerge (PatchSet p1 r1) (PatchSet p2 r2) = PatchSet
    {psPatches = Map.unionWith (<>) p1 p2, psRemovals = Map.unionWith max r1 r2}

instance Monoid PatchSet where
    mempty = PatchSet{psPatches = mempty, psRemovals = mempty}

patchSetFromRawOp :: Op -> PatchSet
patchSetFromRawOp op@Op{opEvent, opRef, opPayload} = case opPayload of
    [] ->  -- remove op
        mempty{psRemovals = Map.singleton opRef opEvent}
    _:_ ->  -- append op
        mempty
            { psPatches =
                Map.singleton
                    opRef
                    VertexList
                        { listHead = opEvent
                        , listItems =
                            HashMap.singleton
                                opEvent
                                VertexListItem
                                    { itemValue = op{opRef = Zero}
                                    , itemNext  = Nothing
                                    }
                        }
            }

patchSetFromChunk :: ReducedChunk -> PatchSet
patchSetFromChunk ReducedChunk{rcRef, rcBody} =
    case uuidVersion $ UUID.split rcRef of
        B11 ->
            -- derived event -- rm-patch compatibility
            foldMap patchSetFromRawOp rcBody
        _ ->  -- patch
            case vertexListFromOps rcBody of
                Just patch -> mempty{psPatches = Map.singleton rcRef patch}
                Nothing -> mempty

instance Reducible RgaRaw where
    reducibleOpType = rgaType

    stateFromChunk = RgaRaw . vertexListFromOps

    stateToChunk (RgaRaw rga) = StateChunk (chunkVersion ops) ops where
        ops = vertexListToOps rga

    applyPatches rga (patches, ops) =
        bimap identity patchSetToChunks . reapplyPatchSetToState rga $
        foldMap patchSetFromChunk patches <> foldMap patchSetFromRawOp ops

    reduceUnappliedPatches (patches, ops) =
        patchSetToChunks . reapplyPatchSet $
        foldMap patchSetFromChunk patches <> foldMap patchSetFromRawOp ops

patchSetToChunks :: PatchSet -> Unapplied
patchSetToChunks PatchSet{..} =
    (   [ ReducedChunk{rcVersion = chunkVersion rcBody, ..}
        | (rcRef, vertices) <- Map.assocs psPatches
        , let rcBody = vertexListToOps $ Just vertices
        ]
    ,   [ Op{opEvent = tombstone, opRef = vid, opPayload = []}
        | (vid, tombstone) <- Map.assocs psRemovals
        ]
    )

chunkVersion :: [Op] -> UUID
chunkVersion ops = maximumDef Zero
    [ max vertexId tombstone
    | Op{opEvent = vertexId, opRef = tombstone} <- ops
    ]

reapplyPatchSet :: PatchSet -> PatchSet
reapplyPatchSet ps =
    continue ps [reapplyPatchesToOtherPatches, reapplyRemovalsToPatches]

reapplyPatchSetToState :: RgaRaw -> PatchSet -> (RgaRaw, PatchSet)
reapplyPatchSetToState rga ps =
    continue (rga, ps) [reapplyPatchesToState, reapplyRemovalsToState]

continue :: x -> [x -> Maybe x] -> x
continue x fs = case asum $ map ($ x) fs of
    Nothing -> x
    Just x' -> continue x' fs

reapplyPatchesToState :: (RgaRaw, PatchSet) -> Maybe (RgaRaw, PatchSet)
reapplyPatchesToState (RgaRaw rstate, ps@PatchSet{..}) = case rstate of
    Just VertexList{listHead = targetHead, listItems = targetItems} -> asum
        [ do
            targetItems' <- applyPatch parent patch targetItems
            pure
                ( RgaRaw . Just $ VertexList targetHead targetItems'
                , ps{psPatches = Map.delete parent psPatches}
                )
        | (parent, patch) <- Map.assocs psPatches
        ]
    Nothing -> do
        -- rstate is empty => only virtual 0 node exists
        -- => we can apply only 0 patch
        patch <- psPatches !? Zero
        pure (RgaRaw $ Just patch, ps{psPatches = Map.delete Zero psPatches})

reapplyPatchesToOtherPatches :: PatchSet -> Maybe PatchSet
reapplyPatchesToOtherPatches ps@PatchSet{..} = asum
    [ do
        targetItems' <- applyPatch parent patch targetItems
        pure ps
            { psPatches =
                Map.insert targetParent (VertexList targetHead targetItems') $
                Map.delete parent psPatches
            }
    | (parent, patch) <- Map.assocs psPatches
    , (targetParent, targetPatch) <- Map.assocs psPatches
    , parent /= targetParent
    , let VertexList targetHead targetItems = targetPatch
    ]

applyPatch
    :: UUID
    -> VertexList
    -> HashMap UUID VertexListItem
    -> Maybe (HashMap UUID VertexListItem)
applyPatch parent patch targetItems = case parent of
    Zero -> undefined
    _ -> do
        item@VertexListItem{itemNext} <- HashMap.lookup parent targetItems
        let VertexList next' newItems = case itemNext of
                Nothing   -> patch
                Just next -> VertexList next targetItems <> patch
        let item' = item{itemNext = Just next'}
        pure $ HashMap.insert parent item' targetItems <> newItems

reapplyRemovalsToState :: (RgaRaw, PatchSet) -> Maybe (RgaRaw, PatchSet)
reapplyRemovalsToState (RgaRaw rstate, ps@PatchSet{..}) = do
    VertexList{listHead = targetHead, listItems = targetItems} <- rstate
    asum
        [ do
            targetItems' <- applyRemoval parent tombstone targetItems
            pure
                ( RgaRaw . Just $ VertexList targetHead targetItems'
                , ps{psRemovals = Map.delete parent psRemovals}
                )
        | (parent, tombstone) <- Map.assocs psRemovals
        ]

reapplyRemovalsToPatches :: PatchSet -> Maybe PatchSet
reapplyRemovalsToPatches PatchSet{..} = asum
    [ do
        targetItems' <- applyRemoval parent tombstone targetItems
        pure PatchSet
            { psRemovals = Map.delete parent psRemovals
            , psPatches =
                Map.insert
                    targetParent (VertexList targetHead targetItems') psPatches
            }
    | (parent, tombstone) <- Map.assocs psRemovals
    , (targetParent, targetPatch) <- Map.assocs psPatches
    , let VertexList targetHead targetItems = targetPatch
    ]

applyRemoval
    :: UUID
    -> UUID
    -> HashMap UUID VertexListItem
    -> Maybe (HashMap UUID VertexListItem)
applyRemoval parent tombstone targetItems = do
    item@VertexListItem{itemValue = v@Op{opRef}} <-
        HashMap.lookup parent targetItems
    let item' = item{itemValue = v{opRef = max opRef tombstone}}
    pure $ HashMap.insert parent item' targetItems

merge :: VertexList -> VertexList -> VertexList
merge v1 v2 =
    fromMaybe undefined . vertexListFromOps $
    (merge' `on` vertexListToOps . Just) v1 v2

merge' :: [Vertex] -> [Vertex] -> [Vertex]
merge' [] vs2 = vs2
merge' vs1 [] = vs1
merge' w1@(v1 : vs1) w2@(v2 : vs2) =
    case compare e1 e2 of
        LT -> v2 : merge' w1 vs2
        GT -> v1 : merge' vs1 w2
        EQ -> mergeVertices : merge' vs1 vs2
  where
    Op{opEvent = e1, opRef = tombstone1, opPayload = p1} = v1
    Op{opEvent = e2, opRef = tombstone2, opPayload = p2} = v2

    -- priority of deletion
    mergeVertices = Op
        { opEvent   = e1
        , opRef     = max tombstone1 tombstone2
        , opPayload = maxOn length p1 p2
        }

-- | Name-UUID to use as RGA type marker.
rgaType :: UUID
rgaType = $(UUID.liftName "rga")

-- | Typed RGA
newtype RGA a = RGA [a]
    deriving (Eq)

instance Replicated a => Replicated (RGA a) where encoding = objectEncoding

instance Replicated a => ReplicatedAsObject (RGA a) where
    objectOpType = rgaType

    newObject (RGA items) = collectFrame $ do
        vertexIds <- lift $ getEventUuids $ ls60 $ genericLength items
        ops <- for (zip items vertexIds) $ \(item, vertexId) -> do
            payload <- newRon item
            pure $ Op vertexId Zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (rgaType, oid) $ StateChunk version ops
        pure oid

    getObject obj@Object{..} = do
        StateChunk{..} <- getObjectStateChunk obj
        mItems <- for stateBody $ \Op{..} -> case opRef of
            Zero -> Just <$> fromRon opPayload objectFrame
            _    -> pure Nothing
        pure . RGA $ catMaybes mItems

-- | Replace content of the RGA throug introducing changes detected by
-- 'getGroupedDiffBy'.
edit
    ::  ( Replicated a, ReplicatedAsPayload a
        , ReplicaClock m, MonadError String m, MonadState (Object (RGA a)) m
        )
    => [a] -> m ()
edit newItems = do
    obj@Object{..} <- get
    StateChunk{..} <- liftEither $ getObjectStateChunk obj
    advanceToUuid stateVersion

    let newItems' = [Op Zero Zero $ toPayload item | item <- newItems]
    let diff = getGroupedDiffBy eqAliveOnPayload stateBody newItems'
    (stateBody', Last lastEvent) <- runWriterT . fmap fold . for diff $ \case
        First removed -> for removed $ \case
            op@Op{opRef = Zero} -> do  -- not deleted yet
                -- TODO(2018-11-03, #15, cblp) get sequential ids
                tombstone <- lift getEventUuid
                tell . Last $ Just tombstone
                pure op{opRef = tombstone}
            op ->  -- deleted already
                pure op
        Both v _      -> pure v
        Second added  -> for added $ \op -> do
            -- TODO(2018-11-03, #15, cblp) get sequential ids
            opEvent <- lift getEventUuid
            tell . Last $ Just opEvent
            pure op{opEvent}

    case lastEvent of
        Nothing -> pure ()
        Just stateVersion' -> do
            let state' = StateChunk stateVersion' stateBody'
            put Object
                { objectFrame =
                    Map.insert (rgaType, objectId) state' objectFrame
                , ..
                }

  where
    eqAliveOnPayload
            Op{opRef = Zero, opPayload = p1}
            Op{opRef = Zero, opPayload = p2}
        = p1 == p2
    eqAliveOnPayload _ _ = False

-- | Speciaization of 'edit' for 'Text'
editText
    :: (ReplicaClock m, MonadError String m, MonadState (Object RgaString) m)
    => Text -> m ()
editText = edit . Text.unpack

-- | Speciaization of 'RGA' to 'Char'.
-- This is the recommended way to store a string.
type RgaString = RGA Char

-- | Create an RGA from a list
newFromList :: (Replicated a, ReplicaClock m) => [a] -> m (Object (RGA a))
newFromList = newObject . RGA

-- | Create an 'RgaString' from a text
newFromText :: ReplicaClock m => Text -> m (Object RgaString)
newFromText = newFromList . Text.unpack

-- | Read elements from RGA
getList :: Replicated a => Object (RGA a) -> Either String [a]
getList = coerce . getObject

-- | Read characters from 'RgaString'
getText :: Object RgaString -> Either String Text
getText = fmap Text.pack . getList
