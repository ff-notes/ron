{-# LANGUAGE NamedFieldPuns #-}

module RON.Data.ORSet.Experimental (add, add_) where

import           RON.Prelude

import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Store (MonadStore, appendPatch)
import           RON.Types (Op (..), Payload, UUID)
import           RON.Types.Experimental (ObjectRef (..))

-- | Add value to the set. Return the reference to the set item.
add :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m UUID
add (ObjectRef collection object) payload = do
  opId <- getEventUuid
  appendPatch collection object [Op{opId, refId = object, payload}]
  pure opId

-- | Add value to the set.
add_ :: (MonadStore m, ReplicaClock m) => ObjectRef a -> Payload -> m ()
add_ objectRef payload = void $ add objectRef payload
