{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           System.Environment (getArgs, getProgName)

import           RON.Data.ORSet (ORSetRep)
import           RON.Store (createObject, getObjects)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types.Experimental (CollectionName)

type Message = ORSetRep

messagesCollection :: CollectionName
messagesCollection = "messages"

main :: IO ()
main = do
  db <- newHandle "./data"
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> do
      messagesResult <- runStore db $ getObjects @Message messagesCollection
      print messagesResult
    [_name, _text] -> do
      messageRef <- runStore db $ createObject @Message messagesCollection
      putStrLn $ "created message: " <> show messageRef
    _ ->
      putStrLn $
      unlines
        [ "Usage:"
        , ""
        , unwords [progName]
        , "\t show all messages"
        , ""
        , unwords [progName, "NAME TEXT"]
        , "\t post a message"
        ]
