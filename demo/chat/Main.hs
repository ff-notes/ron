{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Text as Text
import           Data.Traversable (for)
import           System.Environment (getArgs, getProgName)

import           RON.Data.ORSet (ORSetRep)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Store (listObjects, loadObjectLog, newObject, readObject)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types (Atom (AString))
import           RON.Types.Experimental (CollectionName, ObjectRef (..))

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
      messagesResult <-
        runStore db $ do
          messageRefs <- listObjects @Message messagesCollection
          for messageRefs $ \(ObjectRef _collection messageRef) ->
            loadObjectLog "messages" messageRef
            -- readObject messageRef
      print messagesResult
    [username, text] -> do
      messageRef <-
        runStore db $ do
          messageRef <- newObject @Message messagesCollection
          ORSet.add_
            messageRef
            [AString "username", AString $ Text.pack username]
          ORSet.add_ messageRef [AString "text", AString $ Text.pack text]
          pure messageRef
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
