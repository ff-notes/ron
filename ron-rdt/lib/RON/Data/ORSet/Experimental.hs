{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet.Experimental (
  -- * Untyped
  ORSetRep,
  add,
  add_,
  lookupLww,
  -- * Typed
  ORSet,
  toList,
) where

import           RON.Prelude hiding (toList)

import qualified Data.Map.Strict as Map

import           RON.Data.Experimental (Rep, Replicated, ReplicatedObject,
                                        replicatedTypeId, stateFromFrame, view)
import           RON.Data.ORSet (setType)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Store (MonadStore, appendPatch)
import           RON.Types (Atom, ObjectRef (..), Op (..), Payload, UUID)

-- | Untyped OR-Set.
-- Implementation: a map from the itemId to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
newtype ORSetRep = ORSetRep (Map UUID (UUID, Payload))
  deriving (Eq, Show)

instance Replicated ORSetRep where
  replicatedTypeId = setType
  stateFromFrame objectId = ORSetRep . \case
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

instance ReplicatedObject ORSetRep where
  type Rep ORSetRep = ORSetRep
  view _id = pure

-- | Add value to the set. Return the reference to the set item.
add :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m UUID
add (ObjectRef object) payload = do
  opId <- getEventUuid
  appendPatch object [Op{opId, refId = object, payload}]
  pure opId

-- | Add value to the set.
add_ :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m ()
add_ objectRef payload = void $ add objectRef payload

lookupLww ::
  -- | Key
  Atom ->
  ORSetRep ->
  Maybe Payload
lookupLww key (ORSetRep rep) =
  snd
  <$> maximumMayOn
        fst
        [(item, value) | (item, k : value) <- Map.elems rep, k == key]

toList :: ORSetRep -> [Payload]
toList (ORSetRep rep) = [payload | (_item, payload@(_:_)) <- Map.elems rep]

newtype ORSet a = ORSet ORSetRep

instance ReplicatedObject (ORSet a) where
  type Rep (ORSet a) = ORSetRep
  view _id = pure . ORSet
