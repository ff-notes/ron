{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           RON.Store (getObjects)
import           RON.Store.Test (emptyDB, runStoreSim)
import           RON.Types.Experimental (CollectionName)

data Message

messagesCollection :: CollectionName
messagesCollection = "messages"

main :: IO ()
main = do
    let messagesResult =
            runStoreSim emptyDB $ getObjects @_ @Message messagesCollection
    print messagesResult
