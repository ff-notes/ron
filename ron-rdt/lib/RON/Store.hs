{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  newObject,
  listObjects,
  readObject,
  ) where

import           RON.Prelude

import           RON.Data.Experimental (Rep, ReplicatedObject, replicatedTypeId,
                                        stateFromFrame, view)
import           RON.Error (MonadE, errorContext)
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
  (MonadStore m, ReplicatedObject a, ReplicaClock m) =>
  CollectionName -> m (ObjectRef a)
newObject collection = do
  objectId <- getEventUuid
  let typeId = replicatedTypeId @(Rep a)
  let initOp = Op{opId = objectId, refId = typeId, payload = []}
  appendPatch collection objectId [initOp]
  pure $ ObjectRef collection objectId

readObject ::
  (MonadE m, MonadStore m, ReplicatedObject a, Typeable a) => ObjectRef a -> m a
readObject object@(ObjectRef collection objectId) =
  errorContext ("readObject " <> show object) $ do
    logsByReplicas <- loadObjectLog collection objectId
    view $ stateFromFrame $ sortOn opId $ fold logsByReplicas
