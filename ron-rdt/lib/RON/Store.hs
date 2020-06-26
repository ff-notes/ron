{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  createObject,
  getObjects,
  ) where

import           RON.Prelude

import           RON.Data (Reducible, reducibleOpType)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (Op (..), UUID)
import           RON.Types.Experimental (CollectionName, ObjectRef (..))

class Monad m => MonadStore m where

  -- | Get all collections in database.
  getCollections :: m [CollectionName]

  {- |
    Get all object ids in a collection.
    Must return @[]@ for non-existent collection.
    -}
  getObjectsImpl :: CollectionName -> m [UUID]

  -- {- |
  --   Load object by ref. If object doesn't exist, return 'Nothing'.
  --   @Just []@ means existing but empty object.
  --   -}
  -- loadCachedObjectImpl :: CollectionName -> UUID -> m (Maybe WireStateChunk)

  -- | Add a sequence of operations to an existing object
  addPatch :: CollectionName -> UUID -> [Op] -> m ()

-- loadObjectChunk :: ObjectRef a -> m (Maybe (StateChunk a))
-- loadObjectChunk =
--     loadCachedObjectImpl
--     -- TODO check if there are logs newer than cached object state

-- {- |
--     Load object checking it exists.
--     We cannot assume it exists and return empty if it is not found locally,
--     because we must detect yet not loaded object.
--     -}
-- loadObjectChunk' ::
--     (MonadE m, MonadStore m, Typeable a) => ObjectRef a -> m (StateChunk a)
-- loadObjectChunk' ref = do
--     mobject <- loadObjectChunk ref
--     case mobject of
--         Just obj -> pure obj
--         Nothing  ->
--             throwErrorString $
--                 "loadObjectChunk': Object " ++ show ref ++ " not found"

-- loadObject' :: ObjectRef a -> m (Rep a)
-- loadObject' = undefined

-- -- | Convenient function that saves us from forgotten 'savePatchAndObjectChunk'.
-- modifyObjectChunk_ ::
--     (MonadE m, MonadStore m, Typeable a) =>
--     (StateChunk a -> m (Seq Op, StateChunk a)) -> ObjectRef a -> m ()
-- modifyObjectChunk_ f ref =
--     loadObjectChunk' ref >>= f >>= savePatchAndObjectChunk ref

{- |
  Get all object ids in a collection.
  Returns @[]@ for non-existent collection.
  -}
getObjects :: forall a m. MonadStore m => CollectionName -> m [ObjectRef a]
getObjects collection = map (ObjectRef collection) <$> getObjectsImpl collection

createObject ::
  forall a m.
  (MonadStore m, Reducible a, ReplicaClock m) =>
  CollectionName -> m (ObjectRef a)
createObject collection = do
  objectId <- getEventUuid
  let type_ = reducibleOpType @a
  let initOp = Op{opId = objectId, refId = type_, payload = []}
  addPatch collection objectId [initOp]
  pure $ ObjectRef collection objectId
