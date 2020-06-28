{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, (<>=))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           RON.Error (Error, liftMaybe)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op, UUID)

newtype Object = Object
  { logs :: ObjectLogs
  }
  deriving (Eq, Generic, Show)

type ObjectId = UUID

type ObjectLogs = HashMap ReplicaId (Seq Op)

type TestDB = Map ObjectId Object

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
      emptyObject = Object{logs = HashMap.empty}

  loadObjectLog objectId = do
    db <- StoreSim get
    Object{logs} <- liftMaybe "object not found" $ db !? objectId
    pure $ map toList $ toList logs
