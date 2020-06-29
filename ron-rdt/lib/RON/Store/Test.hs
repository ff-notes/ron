{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, zoom, (<>=))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.String (fromString)

import           RON.Data.Experimental (Rep, ReplicatedObject, replicatedTypeId)
import           RON.Error (Error (..), liftMaybe)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (ObjectRef (..), Op (..), UUID)

data Object = Object
  { init :: Maybe Op
  , logs :: HashMap ReplicaId (Seq Op)
  }
  deriving (Eq, Generic, Show)

type TestDB = Map UUID Object

emptyDB :: TestDB
emptyDB = Map.empty

newtype StoreSim a = StoreSim (StateT TestDB (ReplicaSimT (Either Error)) a)
  deriving (Applicative, Functor, Monad, MonadError Error, ReplicaClock)

runStoreSim :: TestDB -> StoreSim a -> Either Error (a, TestDB)
runStoreSim db (StoreSim action) =
  runNetworkSimT $ runReplicaSimT thisReplicaId $ runStateT action db

thisReplicaId :: ReplicaId
thisReplicaId = applicationSpecific 2020

instance MonadStore StoreSim where
  listObjectsImpl = StoreSim $ gets Map.keys

  appendPatch objectId patch =
    StoreSim $ atObject . #logs . atReplica <>= Seq.fromList patch
    where
      atObject    = at objectId . non emptyObject
      atReplica   = at thisReplicaId . non Seq.empty
      emptyObject = Object{init = Nothing, logs = HashMap.empty}

  loadObjectLog objectId = do
    db <- StoreSim get
    Object{logs} <- liftMaybe "object not found" $ db !? objectId
    pure $ map toList $ toList logs

  openGlobalObject = openGlobalObjectTest

openGlobalObjectTest ::
  forall a. ReplicatedObject a => UUID -> StoreSim (ObjectRef a)
openGlobalObjectTest objectId = do
  StoreSim $
    at objectId `zoom` do
      get >>= \case
        Just Object{init} ->
          -- check type
          when (init /= Just canonicalInitOp) $
            throwError $
            Error
              "Bad init"
              [ fromString $ "got "      <> show init
              , fromString $ "expected " <> show canonicalInitOp
              ]
        Nothing ->
          -- create
          put $ Just Object{init = Just canonicalInitOp, logs = mempty}
  pure (ObjectRef objectId)
  where
    canonicalInitOp =
      Op{opId = objectId, refId = replicatedTypeId @(Rep a), payload = []}
