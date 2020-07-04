module RON.Store.Class (MonadStore (..)) where

import           RON.Prelude

import           RON.Data.VersionVector (VersionVector)
import           RON.Types (Op, UUID)

class Monad m => MonadStore m where

  -- | Get list of all object ids in the database.
  listObjects :: m [UUID]

  {- |
    Append a sequence of operations to an existing object.
    Must have the same origin.
    -}
  appendPatch :: UUID -> [Op] -> m ()

  -- | Get all object logs split by replicas. Replicas order is not guaranteed.
  loadObjectLog ::
    -- | Object id
    UUID ->
    -- | Base version. To get full object logs, pass 'mempty'.
    VersionVector ->
    m [[Op]]
