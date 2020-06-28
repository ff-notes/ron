{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import           Prelude hiding (show)
import           RON.Prelude (show, sortOn)

import           Control.Lens (to, (^?))
import           Data.Generics.Labels ()
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Traversable (for)
import           System.Environment (getArgs, getProgName)
import           Text.Pretty.Simple (pPrint)

import           RON.Data.Experimental (Rep, ReplicatedObject, view)
import           RON.Data.ORSet.Experimental (ORSetRep)
import qualified RON.Data.ORSet.Experimental as ORSet
import qualified RON.Epoch as Epoch
import           RON.Error (MonadE, errorContext, liftMaybe, throwErrorText)
import           RON.Event (ReplicaClock, decodeEvent)
import           RON.Store (MonadStore, listObjects, newObject, readObject)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types (Atom (AString))
import           RON.Types.Experimental (CollectionName, ObjectRef)

data Message = Message
  { postTime :: UTCTime
  , username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject Message where
  type Rep Message = ORSetRep

  view objectId orset =
    errorContext "view @Message" $ do
      postTime <-
        liftMaybe "decode objectId" $
        decodeEvent objectId ^? #localTime . #_TEpoch . to Epoch.decode
      username <- lookupLwwText "username" orset
      text     <- lookupLwwText "text" orset
      pure Message{..}

newMessage ::
  (MonadStore m, ReplicaClock m) => Text -> Text -> m (ObjectRef Message)
newMessage username text = do
  obj <- newObject messagesCollection
  ORSet.add_ obj ["username", AString username]
  ORSet.add_ obj ["text",     AString text    ]
  pure obj

lookupLwwText :: MonadE m => Atom -> ORSetRep -> m Text
lookupLwwText key orset = do
  payload <- liftMaybe ("lookup " <> show key) $ ORSet.lookupLww key orset
  case payload of
    AString text : _ -> pure text
    _ ->
      throwErrorText $ "Value at " <> show key <> " is expected to be a string"

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
          messageRefs <- listObjects messagesCollection
          for messageRefs readObject
      pPrint $ sortOn postTime messagesResult
    [username, text] -> do
      messageRef <-
        runStore db $ newMessage (Text.pack username) (Text.pack text)
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
