{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, (<>=))
import           Data.Generics.Labels ()
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           RON.Data.VersionVector ((·≼))
import           RON.Error (Error (..), liftMaybe)
import           RON.Event (OriginVariety (ApplicationSpecific), Replica,
                            ReplicaClock, mkReplica)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (Patch (..))

newtype Object = Object{logs :: Map Replica (Seq Op)}
  deriving (Eq, Generic, Show)

type TestDB = Map UUID Object

emptyDB :: TestDB
emptyDB = Map.empty

newtype StoreSim a = StoreSim (StateT TestDB (ReplicaSimT (Either Error)) a)
  deriving (Applicative, Functor, Monad, MonadError Error, ReplicaClock)

runStoreSim :: TestDB -> StoreSim a -> Either Error (a, TestDB)
runStoreSim db (StoreSim action) =
  runNetworkSimT $ runReplicaSimT thisReplicaId $ runStateT action db

thisReplicaId :: Replica
thisReplicaId = mkReplica ApplicationSpecific 2020

instance MonadStore StoreSim where
  listObjects = StoreSim $ gets Map.keys

  appendPatch Patch{object, log} =
    StoreSim $ atObject . #logs . atReplica <>= Seq.fromList (toList log)
    where
      atObject    = at object . non emptyObject
      atReplica   = at thisReplicaId . non Seq.empty

  loadWholeObjectLog objectId version = do
    db <- StoreSim get
    Object{logs} <- liftMaybe "object not found" $ db !? objectId
    pure $
      fold
        [ filter (not . isKnown) $ toList @Seq replicaLog
        | replicaLog <- Map.elems logs
        ]

    where
      isKnown Op{opId} = opId ·≼ version

emptyObject :: Object
emptyObject = Object{logs = Map.empty}
