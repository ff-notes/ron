{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, (<>=))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           RON.Error (Error)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op, UUID)
import           RON.Types.Experimental (CollectionName)

type Collection = Map ObjectId Object

newtype Object = Object
  { logs        :: ObjectLogs
  -- , cachedState :: Maybe WireStateChunk
  }
  deriving (Eq, Generic, Show)

type ObjectId = UUID

type ObjectLogs = HashMap ReplicaId (Seq Op)

newtype TestDB = TestDB{collections :: Map CollectionName Collection}
  deriving (Generic, Show)

emptyDB :: TestDB
emptyDB = TestDB{collections = Map.empty}

newtype StoreSim a = StoreSim (StateT TestDB (ReplicaSimT (Either Error)) a)
  deriving (Applicative, Functor, Monad, MonadError Error, ReplicaClock)

runStoreSim :: TestDB -> StoreSim a -> Either Error (a, TestDB)
runStoreSim db (StoreSim action) =
    runNetworkSimT $ runReplicaSimT thisReplicaId $ runStateT action db

thisReplicaId :: ReplicaId
thisReplicaId = applicationSpecific 2020

instance MonadStore StoreSim where
  getCollections = StoreSim $ gets $ Map.keys . collections

  getObjectsImpl collection = do
    TestDB{collections} <- StoreSim get
    pure $ Map.keys $ collections !. collection

  -- loadCachedObjectImpl collection objectId = do
  --   TestDB{collections} <- StoreSim get
  --   pure $ cachedState =<< Map.lookup objectId (collections !. collection)

  addPatch collectionName objectId patch =
    StoreSim $ collection . object . #logs . replica <>= Seq.fromList patch
    where
      collection  = #collections . at collectionName . non Map.empty
      object      = at objectId . non emptyObject
      replica     = at thisReplicaId . non Seq.empty
      emptyObject = Object{{- cachedState = Nothing, -} logs = HashMap.empty}

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
