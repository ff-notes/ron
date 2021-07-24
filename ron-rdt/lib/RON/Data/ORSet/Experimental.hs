{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet.Experimental (
  ORSet,
  ORMap,
  add,
  add_,
  empty,
  lookupLww,
  lookupLww',
  lookupSet,
  toList,
) where

import           RON.Prelude hiding (toList)

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL

import           RON.Data.Experimental (AsAtom, AsAtoms, Rep, Replicated,
                                        ReplicatedObject, fromAtoms,
                                        replicatedTypeId, stateFromFrame,
                                        toAtom, toAtoms, view)
import           RON.Data.ORSet (setType)
import           RON.Error (MonadE, liftMaybe)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Store.Class (MonadStore, appendPatch)
import           RON.Text.Serialize (serializeAtom)
import           RON.Types (Op (..), Payload, UUID)
import           RON.Types.Experimental (Patch (..), Ref (..))

-- | Observed-Remove Set.
-- Implementation: a map from the itemId to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
newtype ORSet a = ORSet (Map UUID (UUID, Payload))
  deriving (Eq, Show)

instance Replicated (ORSet a) where

  replicatedTypeId = setType

  stateFromFrame objectId = ORSet . \case
    [] -> Map.empty
    ops ->
      Map.fromListWith
        (maxOn fst)
        [ (itemId, (opId, payload))
        | Op{opId, refId, payload} <- ops
        , opId /= objectId
        , let
          itemId
            | refId == objectId = opId
            | otherwise         = refId
        ]

instance ReplicatedObject (ORSet a) where
  type Rep (ORSet a) = ORSet a
  view _id = pure

-- | Add value to the set. Return the reference to the set item.
add ::
  (Rep container ~ ORSet item, AsAtoms item, MonadStore m, ReplicaClock m) =>
  Ref container -> item -> m UUID
add (Ref object path) value = do
  advanceToUuid object
  opId <- getEventUuid
  appendPatch
    Patch
      { object
      , log = Op{opId, refId = object, payload = path ++ toAtoms value} :| []
      }
  pure opId

{- |
  Add value to the set or map.

  @add_ :: Ref (ORSet a)   -> a      -> m ()@
  @add_ :: Ref (ORMap k v) -> (k, v) -> m ()@
  -}
add_ ::
  (Rep container ~ ORSet item, AsAtoms item, MonadStore m, ReplicaClock m) =>
  Ref container -> item -> m ()
add_ ref payload = void $ add ref payload

toList :: (AsAtoms a, MonadE m) => ORSet a -> m [a]
toList (ORSet rep) =
  traverse fromAtoms [payload | (_item, payload@(_:_)) <- Map.elems rep]

-- loadSet :: Ref (ORSet a) -> m (Set a)
-- loadSet ref = do
--   ops <- loadObjectLog ref
  -- ORSet
  -- stateFromFrame objectId = ORSet . \case
    -- [] -> Map.empty
    -- ops ->
    --   Map.fromListWith
    --     (maxOn fst)
    --     [ (itemId, (opId, payload))
    --     | Op{opId, refId, payload} <- ops
    --     , opId /= objectId
    --     , let
    --       itemId
    --         | refId == objectId = opId
    --         | otherwise         = refId
    --     ]

type ORMap k v = ORSet (k, v)

lookupLww :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m (Maybe v)
lookupLww key (ORSet s) = do
  traverse fromAtoms $
    snd <$>
    maximumMayOn
      fst
      [(item, value) | (item, k : value) <- Map.elems s, k == toAtom key]

-- | Like 'lookupLww' but assert that key exists.
lookupLww' :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k Payload -> m v
lookupLww' key obj =
  do
    mAtoms <- lookupLww key obj
    atoms  <- liftMaybe ("key " <> ashow key <> " must present") mAtoms
    fromAtoms atoms
  where
    ashow = TextL.toStrict . TextL.decodeUtf8 . serializeAtom . toAtom

empty :: ORSet a
empty = ORSet Map.empty

lookupSet :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m [v]
lookupSet key (ORSet s) =
  traverse
    fromAtoms
    [value | (_item, k : value) <- Map.elems s, k == toAtom key]
