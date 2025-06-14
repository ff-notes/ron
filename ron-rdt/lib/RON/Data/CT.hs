{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Causal Tree (CT)
module RON.Data.CT (
    CT (..),
    CTRep,
    CTString,
    edit,
    editText,
    getAliveWithIndices,
    aliveWithIndices,
    getList,
    getText,
    insert,
    insertAtBegin,
    insertText,
    insertTextAtBegin,
    newFromList,
    newFromText,
    remove,
    ctType,
    RON.Data.CT.toList,
    toText,
)
where

import RON.Prelude

import Control.Monad.State (state)
import Data.Algorithm.Diff (PolyDiff (Both, First, Second), getGroupedDiffBy)
import Data.Foldable qualified as Foldable
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import RON.Data.Internal (
    MonadObjectState,
    Reducible,
    Rep,
    Replicated (encoding),
    ReplicatedAsObject,
    ReplicatedAsPayload,
    fromRon,
    getObjectStateChunk,
    modifyObjectStateChunk_,
    newObject,
    newRon,
    objectEncoding,
    readObject,
    reducibleOpType,
    stateFromChunk,
    stateToChunk,
    toPayload,
 )
import RON.Error (MonadE, errorContext)
import RON.Event (ReplicaClock, getEventUuid, getEventUuids)
import RON.Semilattice (Semilattice)
import RON.Types (
    ObjectRef (ObjectRef),
    Op (Op, opId, payload, refId),
    Payload,
    StateChunk (StateChunk),
    StateFrame,
    UUID,
    WireStateChunk (WireStateChunk, stateBody, stateType),
 )
import RON.UUID (zero)
import RON.UUID qualified as UUID
import RON.Util.Word (ls60)

data CTRepItem = CTRepItem
    { op :: Op
    , next :: Maybe UUID
    -- ^ Nothing is the end
    , isDeleted :: Bool
    -- ^ is tombstone or has tombstone among children
    }
    deriving (Eq, Show)

-- | Untyped Causal Tree
data CTRep = CTRep
    { ops :: Map UUID CTRepItem
    -- ^ indexed by opId
    , start :: Maybe UUID
    }
    deriving anyclass (Semilattice)
    deriving stock (Eq, Show)

instance Semigroup CTRep where
    c <> d = CTRep{ops = c.ops <> d.ops, start = min c.start d.start}

instance Monoid CTRep where
    mempty = CTRep{ops = mempty, start = Nothing}

instance Reducible CTRep where
    reducibleOpType = ctType
    stateFromChunk = ctRepFromChunk
    stateToChunk CTRep{ops} = map op $ Foldable.toList ops

ctRepFromChunk :: [Op] -> CTRep
ctRepFromChunk ops = CTRep{ops = items, start}
  where
    opIndex = Map.fromList [(op.opId, op) | op <- ops]

    -- from parent to children
    childrenIndex =
        Map.fromListWith (<>) [(op.refId, Set.singleton op.opId) | op <- ops]

    roots = childrenIndex !? zero ?: Set.empty

    traversed = traverseFromNodes roots
      where
        traverseFromNodes = foldMap traverseFromNode . reverse . Foldable.toList
        traverseFromNode p =
            p : traverseFromNodes (childrenIndex !? p ?: Set.empty)

    items =
        Map.fromList
            [ (opId, CTRepItem{op, next, isDeleted})
            | (opId, next) <-
                zip traversed (map Just (drop 1 traversed) ++ [Nothing])
            , let op =
                    opIndex
                        !? opId
                        ?: error
                            ( "CTRep: op "
                                <> show opId
                                <> " not found in "
                                <> show opIndex
                            )
            , let children = childrenIndex !? opId ?: Set.empty
            , let isDeleted =
                    null op.payload
                        || any
                            ( \c ->
                                maybe False (\d -> null d.payload) $
                                    opIndex !? c
                            )
                            children
            ]

    start = Set.lookupMax roots

-- | Name-UUID to use as CT type marker.
ctType :: UUID
ctType = $(UUID.liftName "ct")

-- | Typed CT
newtype CT a = CT [a]
    deriving (Eq, Show)

instance (Replicated a) => Replicated (CT a) where encoding = objectEncoding

instance (Replicated a) => ReplicatedAsObject (CT a) where
    type Rep (CT a) = CTRep

    newObject (CT items) = do
        oid <- getEventUuid
        opIds <- getEventUuids $ ls60 $ genericLength items
        ops <-
            for (zip3 items opIds (zero : opIds)) \(item, opId, refId) -> do
                payload <- newRon item
                pure Op{opId, refId, payload}
        modify' $ Map.insert oid $ wireStateChunk ops
        pure $ ObjectRef oid

    readObject = do
        StateChunk stateBody <- getObjectStateChunk
        fmap CT $
            traverse (fromRon . snd) $
                aliveWithIndices $
                    stateFromChunk stateBody

{- | Replace content of the CT through introducing changes detected by
'getGroupedDiffBy'.
-}
edit ::
    ( ReplicatedAsPayload a
    , ReplicaClock m
    , MonadE m
    , MonadObjectState (CT a) m
    ) =>
    [a] ->
    m ()
edit newItems =
    modifyObjectStateChunk_ \(StateChunk stateBody) -> do
        let oldItems' = aliveWithIndices $ stateFromChunk stateBody
        -- TODO(2019-04-17, #59, cblp) replace 'toPayload' with 'newRon' and
        -- relax constraint on 'a' from 'ReplicatedAsPayload' to
        -- 'Replicated'
        let newItems' = map toPayload newItems
        let diff =
                getGroupedDiffBy
                    (\(_, p1) p2 -> p1 == p2)
                    oldItems'
                    newItems'
        newOps <-
            (`evalStateT` zero {- prevous op id -}) $
                fold <$> for diff \case
                    First removed -> do
                        opIds <- getEventUuids $ genericLength removed
                        for (zip removed opIds) \((refId, _), opId) -> do
                            put opId
                            pure Op{opId, refId, payload = []}
                    Both xs _ -> do
                        for_ (lastMay xs) \(opId, _payload) -> put opId
                        pure []
                    Second added -> do
                        opIds <- getEventUuids $ genericLength added
                        for (zip added opIds) \(item, opId) -> do
                            refId <- state (,opId)
                            pure Op{opId, refId, payload = item}
        pure $ StateChunk $ stateBody <> newOps

-- | Speciaization of 'edit' for 'Text'
editText ::
    (ReplicaClock m, MonadE m, MonadObjectState CTString m) => Text -> m ()
editText = edit . Text.unpack

{- | Speciaization of 'CT' to 'Char'.
This is the recommended way to store a string.
-}
type CTString = CT Char

-- | Create a CT from a list
newFromList ::
    (Replicated a, MonadState StateFrame m, ReplicaClock m) =>
    [a] ->
    m (ObjectRef (CT a))
newFromList = newObject . CT

-- | Create a 'CTString' from a text
newFromText ::
    (MonadState StateFrame m, ReplicaClock m) =>
    Text ->
    m (ObjectRef CTString)
newFromText = newFromList . Text.unpack

-- | Read elements from CT
getList :: (Replicated a, MonadE m, MonadObjectState (CT a) m) => m [a]
getList = coerce <$> readObject

-- | Read characters from 'CTString'
getText :: (MonadE m, MonadObjectState CTString m) => m Text
getText = Text.pack <$> getList

{- | Insert a sequence of elements after the specified position.
Position is identified by 'UUID'. 'Nothing' means the beginning.
-}
insert ::
    (Replicated a, MonadE m, MonadObjectState (CT a) m, ReplicaClock m) =>
    [a] ->
    -- | position
    UUID ->
    m ()
insert [] _ = pure ()
insert items startRef =
    modifyObjectStateChunk_ \(StateChunk stateBody) -> do
        opIds <- getEventUuids $ ls60 $ genericLength items
        ops <-
            for (zip3 items opIds (startRef : opIds)) \(item, opId, refId) -> do
                payload <- newRon item
                pure Op{opId, refId, payload}
        pure $ StateChunk $ stateBody <> ops

insertAtBegin ::
    (Replicated a, MonadE m, MonadObjectState (CT a) m, ReplicaClock m) =>
    [a] ->
    m ()
insertAtBegin items = insert items zero

{- | Insert a text after the specified position.
Position is identified by 'UUID'. 'zero' means the beginning.
-}
insertText ::
    (ReplicaClock m, MonadE m, MonadObjectState CTString m) =>
    Text ->
    -- | position
    UUID ->
    m ()
insertText = insert . Text.unpack

insertTextAtBegin ::
    (ReplicaClock m, MonadE m, MonadObjectState CTString m) => Text -> m ()
insertTextAtBegin text = insertText text zero

-- | Record a removal of a specific item
remove ::
    (MonadE m, MonadObjectState (CT a) m, ReplicaClock m) =>
    -- | reference to the item to remove
    UUID ->
    m ()
remove refId =
    errorContext "CT.remove" . errorContext ("refId = " <> show refId) $
        modifyObjectStateChunk_ \(StateChunk stateBody) -> do
            opId <- getEventUuid
            let op = Op{opId, refId, payload = []}
            pure $ StateChunk $ stateBody <> [op]

wireStateChunk :: [Op] -> WireStateChunk
wireStateChunk stateBody = WireStateChunk{stateType = ctType, stateBody}

toList :: CT a -> [a]
toList (CT xs) = xs

toText :: CTString -> Text
toText (CT s) = Text.pack s

aliveWithIndices :: CTRep -> [(UUID, Payload)]
aliveWithIndices CTRep{ops, start} = go start
  where
    go = \case
        Nothing -> []
        Just p ->
            case ops !? p of
                Nothing -> []
                Just CTRepItem{op, next, isDeleted} ->
                    [(op.opId, op.payload) | not isDeleted] ++ go next

getAliveWithIndices ::
    (MonadE m, MonadObjectState (CT a) m) =>
    m [(UUID, Payload)]
getAliveWithIndices = do
    StateChunk stateBody <- getObjectStateChunk
    pure $ aliveWithIndices $ stateFromChunk stateBody
