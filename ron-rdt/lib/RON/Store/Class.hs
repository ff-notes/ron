module RON.Store.Class (MonadStore (..)) where

import           RON.Prelude

import           RON.Data.VersionVector (VV)
import           RON.Types (Op, UUID)
import           RON.Types.Experimental (Patch)

class Monad m => MonadStore m where

  -- | Get list of all object ids in the database.
  listObjects :: m [UUID]

  -- | Append a sequence of operations to an object.
  appendPatch :: Patch -> m ()

  -- | Get all RON-object logs split by replicas.
  -- Replicas order is not guaranteed.
  -- Implementation SHOULD return object creation op.
  loadWholeObjectLog ::
    -- | Object id
    UUID ->
    -- | Base version. To get object logs from the beginning, pass 'mempty'.
    VV ->
    m [Op]

  -- | Last version of an object known to the replica.
  getObjectVersion :: UUID -> m VV
