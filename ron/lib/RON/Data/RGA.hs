{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Data.RGA (RGA) where

import           RON.Internal.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (RChunk' (..), Reduced (..), Reducible (..))
import           RON.Internal.Word (pattern B11)
import           RON.Types (Op' (..), UUID)
import           RON.UUID (pattern Zero, uuidScheme)
import qualified RON.UUID as UUID

-- | opEvent = vertex id
--   opRef:
--      0 = value is alive,
--      _ = tombstone event, value is backup for undo
--   opPayload: the value
-- TODO record pattern synonyms
newtype Vertex = Vertex Op'
    deriving (Eq, Show)

unVertex :: Vertex -> Op'
unVertex (Vertex op) = op

data VertexListItem = VertexListItem
    { itemValue :: Vertex
    , itemNext  :: Maybe UUID
    }
    deriving (Eq, Show)

-- | MonoFoldable?
data VertexList = VertexList
    { listHead  :: UUID
    , listItems :: HashMap UUID VertexListItem
    }
    deriving (Eq, Show)

instance Semigroup VertexList where
    (<>) = merge

vertexListToList :: Maybe VertexList -> [Vertex]
vertexListToList mv = case mv of
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

vertexListToOps :: Maybe VertexList -> [Op']
vertexListToOps = map unVertex . vertexListToList

vertexListFromList :: [Vertex] -> Maybe VertexList
vertexListFromList = foldr go mempty where
    go v@(Vertex Op'{opEvent = vid}) vlist =
        Just $ VertexList{listHead = vid, listItems = vlist'}
      where
        item itemNext = VertexListItem{itemValue = v, itemNext}
        vlist' = case vlist of
            Nothing -> HashMap.singleton vid (item Nothing)
            Just VertexList{listHead, listItems} ->
                HashMap.insert vid (item $ Just listHead) listItems

vertexListFromOps :: [Op'] -> Maybe VertexList
vertexListFromOps = vertexListFromList . map Vertex

data RGA = RGA
    { rgaState             :: Maybe VertexList
    , rgaUnappliedPatches  :: Map UUID VertexList
        -- ^ the key is the parent event, the value is a non-empty VertexList
    , rgaUnappliedRemovals :: Map UUID UUID
        -- ^ the key is the target event, the value is the tombstone event
    }
    deriving (Eq, Show)

instance Semigroup RGA where
    rga1 <> rga2 = reapply $ preMerge rga1 rga2

preMerge :: RGA -> RGA -> RGA
preMerge (RGA state1 patches1 rm1) (RGA state2 patches2 rm2) = RGA
    { rgaState             = state1 <> state2
    , rgaUnappliedPatches  = Map.unionWith (<>) patches1 patches2
    , rgaUnappliedRemovals = Map.unionWith max rm1 rm2
    }

instance Monoid RGA where
    mempty = RGA
        { rgaState             = mempty
        , rgaUnappliedPatches  = mempty
        , rgaUnappliedRemovals = mempty
        }

instance Reducible RGA where
    type OpType RGA = "rga"

    fromRawOp op@Op'{opEvent, opRef, opPayload} = case opPayload of
        [] ->  -- remove op
            mempty{rgaUnappliedRemovals = Map.singleton opRef opEvent}
        _:_ ->  -- append op
            mempty
                { rgaUnappliedPatches =
                    Map.singleton
                        opRef
                        VertexList
                            { listHead = opEvent
                            , listItems =
                                HashMap.singleton
                                    opEvent
                                    VertexListItem
                                        { itemValue = Vertex op{opRef = Zero}
                                        , itemNext  = Nothing
                                        }
                            }
                }

    fromChunk ref ops = case ref of
        Zero ->  -- state
            mempty{rgaState = vertexListFromOps ops}
        (uuidScheme . UUID.split -> B11) ->
            -- derived event -- rm-patch compatibility
            foldMap fromRawOp ops
        _ ->  -- patch
            case vertexListFromOps ops of
                Just patch ->
                    mempty{rgaUnappliedPatches = Map.singleton ref patch}
                Nothing -> mempty

    toChunks RGA{..} = Reduced
        { reducedStateVersion = chunkVersion reducedStateBody
        , reducedStateBody
        , reducedPatches =
            [ RChunk'{rchunk'Version = chunkVersion rchunk'Body, ..}
            | (rchunk'Ref, vertices) <- Map.assocs rgaUnappliedPatches
            , let rchunk'Body = vertexListToOps $ Just vertices
            ]
        , reducedUnappliedOps =
            [ Op'{opEvent = tombstone, opRef = vid, opPayload = []}
            | (vid, tombstone) <- Map.assocs rgaUnappliedRemovals
            ]
        }
      where
        reducedStateBody = vertexListToOps rgaState
        chunkVersion ops = maximumDef Zero
            [ max vertexId tombstone
            | Op'{opEvent = vertexId, opRef = tombstone} <- ops
            ]

    sameState = (==) `on` rgaState

reapply :: RGA -> RGA
reapply = go reapplyRemovals reapplyPatches where
    go f1 f2 x = case f1 x of
        Just x1 -> go f1 f2 x1
        Nothing -> case f2 x of
            Just x2 -> go f2 f1 x2
            Nothing -> x

reapplyPatches :: RGA -> Maybe RGA
reapplyPatches rga =
    reapplyPatchesToState rga <|> reapplyPatchesToOtherPatches rga
  where
    reapplyPatchesToState RGA{..} = do
        VertexList{listHead = targetHead, listItems = targetItems} <- rgaState
        asum
            [ do
                targetItems' <- apply parent patch targetItems
                pure RGA
                    { rgaState = Just $ VertexList targetHead targetItems'
                    , rgaUnappliedPatches =
                        Map.delete parent rgaUnappliedPatches
                    , ..
                    }
            | (parent, patch) <- Map.assocs rgaUnappliedPatches
            ]
    reapplyPatchesToOtherPatches RGA{..} = asum
        [ do
            targetItems' <- apply parent patch targetItems
            pure RGA
                { rgaUnappliedPatches =
                    Map.insert
                        targetParent
                        (VertexList targetHead targetItems') $
                    Map.delete parent rgaUnappliedPatches
                , ..
                }
        | (parent, patch) <- Map.assocs rgaUnappliedPatches
        , (targetParent, targetPatch) <- Map.assocs rgaUnappliedPatches
        , parent /= targetParent
        , let VertexList targetHead targetItems = targetPatch
        ]
    apply parent patch targetItems = do
        item@VertexListItem{itemNext} <- HashMap.lookup parent targetItems
        let VertexList next' newItems = case itemNext of
                Nothing   -> patch
                Just next -> VertexList next targetItems <> patch
        let item' = item{itemNext = Just next'}
        pure $ HashMap.insert parent item' targetItems <> newItems

reapplyRemovals :: RGA -> Maybe RGA
reapplyRemovals rga =
    reapplyRemovalsToState rga <|> reapplyRemovalsToPatches rga
  where
    reapplyRemovalsToState RGA{..} = do
        VertexList{listHead = targetHead, listItems = targetItems} <- rgaState
        asum
            [ do
                targetItems' <- apply parent tombstone targetItems
                pure RGA
                    { rgaState = Just $ VertexList targetHead targetItems'
                    , rgaUnappliedRemovals =
                        Map.delete parent rgaUnappliedRemovals
                    , ..
                    }
            | (parent, tombstone) <- Map.assocs rgaUnappliedRemovals
            ]
    reapplyRemovalsToPatches RGA{..} = asum
        [ do
            targetItems' <- apply parent tombstone targetItems
            pure RGA
                { rgaUnappliedRemovals = Map.delete parent rgaUnappliedRemovals
                , rgaUnappliedPatches =
                    Map.insert
                        targetParent
                        (VertexList targetHead targetItems')
                        rgaUnappliedPatches
                , ..
                }
        | (parent, tombstone) <- Map.assocs rgaUnappliedRemovals
        , (targetParent, targetPatch) <- Map.assocs rgaUnappliedPatches
        , let VertexList targetHead targetItems = targetPatch
        ]
    apply parent tombstone targetItems = do
        item@VertexListItem{itemValue = Vertex v@Op'{opRef}} <-
            HashMap.lookup parent targetItems
        let item' = item{itemValue = Vertex v{opRef = max opRef tombstone}}
        pure $ HashMap.insert parent item' targetItems

merge :: VertexList -> VertexList -> VertexList
merge v1 v2 =
    fromMaybe undefined . vertexListFromList $
    (merge' `on` vertexListToList . Just) v1 v2

merge' :: [Vertex] -> [Vertex] -> [Vertex]
merge' [] vs2 = vs2
merge' vs1 [] = vs1
merge' w1@(v1 : vs1) w2@(v2 : vs2) =
    case compare e1 e2 of
        LT -> v2 : merge' w1 vs2
        GT -> v1 : merge' vs1 w2
        EQ -> mergeVertices : merge' vs1 vs2
  where
    Vertex Op'{opEvent = e1, opRef = tombstone1, opPayload = p1} = v1
    Vertex Op'{opEvent = e2, opRef = tombstone2, opPayload = p2} = v2

    -- priority of deletion
    mergeVertices = Vertex Op'
        { opEvent   = e1
        , opRef     = max tombstone1 tombstone2
        , opPayload = maxOn length p1 p2
        }
