{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module RON.Store.Test (runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, zoom, (.=), (<>=))
import           Data.Generics.Labels ()
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import           RON.Error (Error)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op, StateChunk (..), UUID)
import           RON.Types.Experimental (CollectionName, ObjectRef (..))

type Collection = Map ObjectId Object

data Object = Object
    { value :: [Op]
    , logs  :: ObjectLogs
    }
    deriving (Eq, Generic)

emptyObject :: Object
emptyObject = Object{value = [], logs = mempty}

type ObjectId = UUID

type ObjectLogs = HashMap ReplicaId (Seq Op)

newtype TestDB = TestDB
    { collections :: Map CollectionName Collection
    }
    deriving (Generic)

newtype StoreSim a = StoreSim (StateT TestDB (ReplicaSimT (Either Error)) a)
    deriving (Applicative, Functor, Monad, MonadError Error, ReplicaClock)

runStoreSim :: TestDB -> StoreSim a -> Either Error (a, TestDB)
runStoreSim db (StoreSim action) =
    runNetworkSimT $ runReplicaSimT thisReplicaId $ runStateT action db

thisReplicaId :: ReplicaId
thisReplicaId = applicationSpecific 2020

instance MonadStore StoreSim where
    getCollections = StoreSim $ gets $ Map.keys . collections

    getObjects collection =
        StoreSim $ do
            TestDB{collections} <- get
            pure $ map (ObjectRef collection) $ Map.keys $
                collections !. collection

    loadObjectChunk (ObjectRef collection objectId) =
        StoreSim $ do
            TestDB{collections} <- get
            pure $ fmap (StateChunk . \Object{value} -> value) $
                Map.lookup objectId $ collections !. collection

    savePatchAndObjectChunk
        (ObjectRef collection objectId)
        (patch, StateChunk value)
        =
            StoreSim $
                atCollection . atObject `zoom` do
                    #value .= value
                    #logs . at thisReplicaId . non mempty <>= patch
        where
            atCollection = #collections . at collection . non mempty
            atObject = at objectId . non emptyObject

    -- saveVersionContent (DocId docid :: DocId a) version content = do
    --     let document = BSLC.lines content
    --     let insertDocumentVersion =
    --             Just . Map.insertWith (<>) version document . fromMaybe mempty
    --     let alterDocument
    --             = Just
    --             . Map.alter insertDocumentVersion docid
    --             . fromMaybe mempty
    --     let alterCollection = Map.alter alterDocument (collectionName @a)
    --     StorageSim $ modify' alterCollection

    -- loadVersionContent (DocId dir :: DocId a) version = StorageSim $ do
    --     db <- get
    --     pure $ BSLC.unlines $ db !. collectionName @a !. dir ! version

    -- deleteVersion (DocId doc :: DocId a) version
    --     = StorageSim
    --     . modify'
    --     . (`Map.adjust` collectionName @a)
    --     . (`Map.adjust` doc)
    --     $ Map.delete version

    -- changeDocId (DocId old :: DocId a) (DocId new :: DocId a) = StorageSim $
    --     modify' $ (`Map.adjust` collectionName @a) $ \collection ->
    --         maybe collection (uncurry $ Map.insert new) $
    --         mapTake old collection

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
