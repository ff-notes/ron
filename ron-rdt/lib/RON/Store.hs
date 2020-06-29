{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  listObjects,
  newObject,
  openGlobalObject,
  readObject,
  ) where

import           RON.Prelude

import           Data.String (fromString)

import           RON.Data.Experimental (Rep, ReplicatedObject, replicatedTypeId,
                                        stateFromFrame, view)
import           RON.Error (Error (..), MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (ObjectRef (..), Op (..), UUID)

class Monad m => MonadStore m where
  -- | Get list of all object ids in the database.
  listObjectsImpl :: m [UUID]

  -- | Append a sequence of operations to an existing object
  appendPatch :: UUID -> [Op] -> m ()

  -- | Get all object logs split by replicas. Replicas order is not guaranteed.
  loadObjectLog :: UUID -> m [[Op]]

  loadObjectInit :: UUID -> m (Maybe Op)

  saveObjectInit :: UUID -> Op -> m ()

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

-- | Nothing if object doesn't exist in the replica.
readObject ::
  (MonadE m, MonadStore m, ReplicatedObject a, Typeable a) =>
  ObjectRef a -> m (Maybe a)
readObject object@(ObjectRef objectId) =
  errorContext ("readObject " <> show object) $ do
    logsByReplicas <- loadObjectLog objectId
    case logsByReplicas of
      [] -> pure Nothing
      _ ->
        Just <$>
        view
          objectId
          (stateFromFrame objectId $ sortOn opId $ fold logsByReplicas)

openGlobalObject ::
  forall a m. (MonadE m, MonadStore m, ReplicatedObject a) => UUID -> m (ObjectRef a)
openGlobalObject objectId =
  do
    errorContext ("openGlobalObject " <> show objectId) $ do
      loadObjectInit objectId >>= \case
        Just init ->
          when (init /= canonicalInit) $
            throwError $
            Error
              "Bad init"
              [ fromString $ "got "      <> show init
              , fromString $ "expected " <> show canonicalInit
              ]
        Nothing -> saveObjectInit objectId canonicalInit
    pure (ObjectRef objectId)
  where
    canonicalInit =
      Op{opId = objectId, refId = replicatedTypeId @(Rep a), payload = []}
