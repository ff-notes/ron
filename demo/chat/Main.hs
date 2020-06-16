{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           RON.Store (getObjects)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types.Experimental (CollectionName)

data Message

messagesCollection :: CollectionName
messagesCollection = "messages"

main :: IO ()
main = do
  db <- newHandle "./data"
  messagesResult <- runStore db $ getObjects @_ @Message messagesCollection
  print messagesResult
