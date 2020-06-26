{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  newObject,
  listObjects,
  readObject,
  ) where

import           Debug.Trace
import           RON.Prelude

import           RON.Data (Reducible, reducibleOpType, stateFromChunk)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (CollectionName, ObjectRef (..))

class Monad m => MonadStore m where

  -- | Get list of all collections in database.
  listCollections :: m [CollectionName]

  {- |
    Get list of all object ids in a collection.
    Must return @[]@ for non-existent collection.
    -}
  listObjectsImpl :: CollectionName -> m [UUID]

  -- | Append a sequence of operations to an existing object
  appendPatch :: CollectionName -> UUID -> [Op] -> m ()

  -- | Get all object logs split by replicas. Replicas order is not guaranteed.
  loadObjectLog :: CollectionName -> UUID -> m [[Op]]

{- |
  Get list of all object ids in a collection.
  Returns @[]@ for non-existent collection.
  -}
listObjects :: forall a m. MonadStore m => CollectionName -> m [ObjectRef a]
listObjects collection =
  map (ObjectRef collection) <$> listObjectsImpl collection

newObject ::
  forall a m.
  (MonadStore m, Reducible a, ReplicaClock m) =>
  CollectionName -> m (ObjectRef a)
newObject collection = do
  objectId <- getEventUuid
  let type_ = reducibleOpType @a
  let initOp = Op{opId = objectId, refId = type_, payload = []}
  appendPatch collection objectId [initOp]
  pure $ ObjectRef collection objectId

readObject :: (MonadStore m, Reducible a) => ObjectRef a -> m a
readObject (ObjectRef collection objectId) = do
  logsByReplicas <- loadObjectLog collection objectId
  traceShowM logsByReplicas
  pure $ stateFromChunk $ sortOn opId $ fold logsByReplicas
