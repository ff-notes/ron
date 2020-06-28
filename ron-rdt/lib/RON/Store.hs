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
import           RON.Types (ObjectRef (..), Op (..), UUID)

class Monad m => MonadStore m where
  -- | Get list of all object ids in the database.
  listObjectsImpl :: m [UUID]

  -- | Append a sequence of operations to an existing object
  appendPatch :: UUID -> [Op] -> m ()

  -- | Get all object logs split by replicas. Replicas order is not guaranteed.
  loadObjectLog :: UUID -> m [[Op]]

-- | Get list of all object ids in the database.
listObjects :: forall a m. MonadStore m => m [ObjectRef a]
listObjects = map ObjectRef <$> listObjectsImpl

newObject ::
  forall a m.
  (MonadStore m, ReplicatedObject a, ReplicaClock m) => m (ObjectRef a)
newObject = do
  objectId <- getEventUuid
  let typeId = replicatedTypeId @(Rep a)
  let initOp = Op{opId = objectId, refId = typeId, payload = []}
  appendPatch objectId [initOp]
  pure $ ObjectRef objectId

readObject ::
  (MonadE m, MonadStore m, ReplicatedObject a, Typeable a) => ObjectRef a -> m a
readObject object@(ObjectRef objectId) =
  errorContext ("readObject " <> show object) $ do
    logsByReplicas <- loadObjectLog objectId
    view objectId $ stateFromFrame $ sortOn opId $ fold logsByReplicas
