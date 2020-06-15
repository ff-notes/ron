{-# LANGUAGE FlexibleContexts #-}

module RON.Store
    (MonadStore (..), loadObjectChunk', modifyObjectChunk_, loadObject') where

import           RON.Prelude

import           RON.Data.Internal (Rep)
import           RON.Error (MonadE, throwErrorString)
import           RON.Types (ObjectRef, StateChunk)

class Monad m => MonadStore m where
    -- | Load object by ref. If object doesn't exist, return 'Nothing'.
    loadObjectChunk :: ObjectRef a -> m (Maybe (StateChunk a))

    -- | Save new state of the object and patch.
    -- TODO remove in favor of 'savePatchAndObjectChunk'
    saveObjectChunk :: ObjectRef a -> StateChunk a -> m ()

    -- TODO | Save new state of the object and patch.
    -- savePatchAndObjectChunk :: ([Op], StateChunk a) -> ObjectRef a -> m ()

-- | Load object and checking it exists.
-- TODO: we can assume it exists and return empty if it is not found locally.
loadObjectChunk' ::
    (MonadE m, MonadStore m, Typeable a) => ObjectRef a -> m (StateChunk a)
loadObjectChunk' ref = do
    mobject <- loadObjectChunk ref
    case mobject of
        Just obj -> pure obj
        Nothing  ->
            throwErrorString $
                "loadObjectChunk': Object " ++ show ref ++ " not found"

loadObject' :: ObjectRef a -> m (Rep a)
loadObject' = undefined

-- | Convenient function that saves us from forgotten 'saveObjectChunk'.
modifyObjectChunk_ ::
    (MonadE m, MonadStore m, Typeable a) =>
    (StateChunk a -> m (StateChunk a)) -> ObjectRef a -> m ()
modifyObjectChunk_ f ref = loadObjectChunk' ref >>= f >>= saveObjectChunk ref
