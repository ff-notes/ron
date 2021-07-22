module Fork (forkLinked) where

import           RON.Prelude

import           Control.Concurrent.Async (async, link)

forkLinked :: IO () -> IO ()
forkLinked = async >=> link
