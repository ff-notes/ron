{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, (<>=))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           RON.Error (Error (..), liftMaybe)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op (..), UUID)

newtype Object = Object
  { logs :: HashMap ReplicaId (Seq Op)
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
  listObjects = StoreSim $ gets Map.keys

  appendPatch objectId patch =
    StoreSim $ atObject . #logs . atReplica <>= Seq.fromList patch
    where
      atObject    = at objectId . non emptyObject
      atReplica   = at thisReplicaId . non Seq.empty

  loadObjectLog objectId = do
    db <- StoreSim get
    Object{logs} <- liftMaybe "object not found" $ db !? objectId
    pure $ map toList $ toList logs

emptyObject :: Object
emptyObject = Object{logs = HashMap.empty}
