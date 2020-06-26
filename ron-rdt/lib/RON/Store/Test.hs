{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Data.Generics.Labels ()
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import           RON.Error (Error)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op, UUID, WireStateChunk (..))
import           RON.Types.Experimental (CollectionName)

type Collection = Map ObjectId Object

data Object = Object
    { cachedState :: Maybe WireStateChunk
    , logs        :: ObjectLogs
    }
    deriving (Eq, Generic, Show)

-- emptyObject :: Object
-- emptyObject = Object{cachedState = Nothing, logs = HashMap.empty}

type ObjectId = UUID

type ObjectLogs = HashMap ReplicaId (Seq Op)

newtype TestDB = TestDB
    { collections :: Map CollectionName Collection
    }
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

  getObjectsImpl collection =
    StoreSim $ do
      TestDB{collections} <- get
      pure $ Map.keys $ collections !. collection

  loadCachedObjectImpl collection objectId =
    StoreSim $ do
      TestDB{collections} <- get
      pure $ do
        Object{cachedState} <- Map.lookup objectId (collections !. collection)
        cachedState

  -- savePatchAndObjectChunk
  --     (ObjectRef collection objectId)
  --     (patch, StateChunk value)
  --     =
  --         StoreSim $
  --             atCollection . atObject `zoom` do
  --                 #value .= value
  --                 #logs . at thisReplicaId . non Seq.empty <>= patch
  --     where
  --         atCollection = #collections . at collection . non Map.empty
  --         atObject = at objectId . non emptyObject

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
