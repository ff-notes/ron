module RON.Store.Class (MonadStore (..)) where

import           RON.Prelude

import           RON.Data.VersionVector (VV)
import           RON.Types (Op, UUID)

class Monad m => MonadStore m where

  -- | Get list of all object ids in the database.
  listObjects :: m [UUID]

  {- |
    Append a sequence of operations to an object.
    Precondition: all operations must have the same origin.
    -}
  appendPatchFromOneOrigin :: UUID -> [Op] -> m ()

  -- | Get all object logs split by replicas. Replicas order is not guaranteed.
  loadObjectLog ::
    -- | Object id
    UUID ->
    -- | Base version. To get full object logs, pass 'mempty'.
    VV ->
    m [[Op]]

  -- | Last version of an object known to the replica.
  getObjectVersion :: UUID -> m VV
