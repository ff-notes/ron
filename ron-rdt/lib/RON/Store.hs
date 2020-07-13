{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  newObject,
  openNamedObject,
  readGlobalSet,
  readObject,
) where

import           System.Exit (die)
import           System.IO.Unsafe (unsafePerformIO)

import           RON.Prelude

import           RON.Data.Experimental (AsAtoms, Rep, ReplicatedObject,
                                        replicatedTypeId, stateFromFrame, view)
import           RON.Data.ORSet (setType)
import           RON.Data.ORSet.Experimental (ORMap)
import qualified RON.Data.ORSet.Experimental as ORMap
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Store.Class (MonadStore (..))
import           RON.Types (Atom, ObjectRef (..), Op (..), UUID)
import qualified RON.UUID as UUID

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
    ops <- fold <$> loadObjectLog objectId mempty
    case ops of
      [] -> pure Nothing
      _  -> Just <$> view objectId (stateFromFrame objectId $ sortOn opId ops)

-- | Read global variable identified by atom and return result as set.
readGlobalSet ::
  (MonadE m, MonadStore m, AsAtoms a, Typeable a) => Atom -> m [a]
readGlobalSet name =
  errorContext ("readGlobalSet " <> show name) $ do
    mGlobals <- readObject globalsRef
    globals <- case mGlobals of
      Just globals -> pure globals
      Nothing      -> do
        createGlobals
        pure ORMap.empty
    ORMap.lookupSet name globals
  where
    createGlobals =
      appendPatch
        globalsId
        [Op{opId = globalsId, refId = setType, payload = []}]

globalsId :: UUID
globalsId = $(UUID.liftName "globals")

globalsRef :: ObjectRef (ORMap Atom a)
globalsRef = ObjectRef globalsId

openNamedObject ::
  (MonadE m, MonadStore m, ReplicaClock m, ReplicatedObject a, Typeable a) =>
  Atom -> m (ObjectRef a)
openNamedObject name = do
  set <- readGlobalSet name
  case set of
    [obj] -> pure obj
    [] -> do
      obj <- newObject
      ORMap.add_ globalsRef (name, obj)
      pure obj
    _ -> unsafePerformIO $ die "Store.openNamedObject: TODO: merge objects"
