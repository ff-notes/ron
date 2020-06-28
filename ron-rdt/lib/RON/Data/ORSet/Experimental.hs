{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module RON.Data.ORSet.Experimental (ORSetRep, add, add_, lookupLww) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Experimental (Replicated, replicatedTypeId,
                                        stateFromFrame)
import           RON.Data.ORSet (setType)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Store (MonadStore, appendPatch)
import           RON.Types (Atom, Op (..), Payload, UUID)
import           RON.Types.Experimental (ObjectRef (..))

-- | Untyped OR-Set.
-- Implementation: a map from the itemId to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
newtype ORSetRep = ORSetRep (Map UUID (UUID, Payload))
  deriving (Eq, Show)

instance Replicated ORSetRep where
  replicatedTypeId = setType
  stateFromFrame = ORSetRep . \case
    [] -> Map.empty
    Op{opId = object} : ops ->
      Map.fromListWith
        (maxOn fst)
        [ (itemId, (opId, payload))
        | Op{opId, refId, payload} <- ops
        , let
          itemId
            | refId == object = opId
            | otherwise       = refId
        ]

-- | Add value to the set. Return the reference to the set item.
add :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m UUID
add (ObjectRef collection object) payload = do
  opId <- getEventUuid
  appendPatch collection object [Op{opId, refId = object, payload}]
  pure opId

-- | Add value to the set.
add_ :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m ()
add_ objectRef payload = void $ add objectRef payload

lookupLww ::
  -- | Key
  Atom ->
  ORSetRep ->
  Maybe Payload
lookupLww key (ORSetRep set) =
  snd
  <$> maximumMayOn
        fst
        [(item, value) | (item, k : value) <- Map.elems set, k == key]
