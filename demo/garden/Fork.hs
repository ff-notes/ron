module Fork (forkLinked) where

import           RON.Prelude

import           UnliftIO (MonadUnliftIO, async, link)

forkLinked :: MonadUnliftIO m => m any -> m ()
forkLinked = async >=> link
