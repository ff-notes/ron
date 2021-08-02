module Fork (forkLinked) where

import           RON.Prelude

import           UnliftIO (MonadUnliftIO, async, link)

forkLinked :: MonadUnliftIO m => m () -> m ()
forkLinked = async >=> link
