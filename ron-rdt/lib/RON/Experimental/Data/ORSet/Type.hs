module RON.Experimental.Data.ORSet.Type where

import           RON.Prelude

import           RON.Types (Payload, UUID)

-- | Observed-Remove Set.
-- Implementation: a map from the itemId to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
-- Tombstone is an op with empty payload (even without prefix) referencing item.
newtype ORSet a = ORSet (Map UUID (UUID, Payload))
  deriving (Eq, Show)

type ORMap k v = ORSet (k, v)
