{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           System.Environment (getArgs, getProgName)

import           RON.Store (getObjects)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types.Experimental (CollectionName)

data Message

messagesCollection :: CollectionName
messagesCollection = "messages"

main :: IO ()
main = do
  db <- newHandle "./data"
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> do
      messagesResult <- runStore db $ getObjects @_ @Message messagesCollection
      print messagesResult
    [_name, _text] -> undefined
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
