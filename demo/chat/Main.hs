{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Prelude hiding (show)
import           RON.Prelude (show)

import           Control.Lens (to, (^?))
import           Control.Monad.IO.Class (liftIO)
import           Data.Generics.Labels ()
import           Data.List (sortOn)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Traversable (for)
import           System.Environment (getArgs, getProgName)
import           Text.Pretty.Simple (pPrint)

import           RON.Data.Experimental (Rep, ReplicatedObject, fromAtoms, view)
import           RON.Data.ORSet.Experimental (ORMap, ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import qualified RON.Data.ORSet.Experimental as ORMap
import qualified RON.Epoch as Epoch
import           RON.Error (MonadE, errorContext, liftMaybe)
import           RON.Event (ReplicaClock, decodeEvent)
import           RON.Store (MonadStore, newObject, openGlobalObject, readObject)
import           RON.Store.FS (Handle, newHandle, runStore)
import           RON.Types (Atom (AString), ObjectRef (..), Payload, UUID)
import qualified RON.UUID as UUID

data Message = Message
  { postTime :: UTCTime
  , username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject Message where
  type Rep Message = ORMap Text Payload

  view objectId orset =
    errorContext "view @Message" $ do
      postTime <-
        liftMaybe "decode objectId" $
        decodeEvent objectId ^? #localTime . #_TEpoch . to Epoch.decode
      username <- lookupLwwText "username" orset
      text     <- lookupLwwText "text"     orset
      pure Message{..}

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  Text -> Text -> m (ObjectRef Message)
newMessage username text = do
  gMessages :: ObjectRef (ORSet (ObjectRef Message)) <-
    openGlobalObject gMessagesId
  msgRef <- newObject @Message
  ORSet.add_ msgRef ("username", [AString username])
  ORSet.add_ msgRef ("text",     [AString text    ])
  ORSet.add_ gMessages msgRef
  pure msgRef

lookupLwwText :: MonadE m => Text -> ORMap Text Payload -> m Text
lookupLwwText key obj = do
  mAtoms <- ORMap.lookupLww key obj
  atoms  <- liftMaybe ("key " <> key <> " must present") mAtoms
  fromAtoms atoms

gMessagesId :: UUID
gMessagesId = $(UUID.liftName "messages")

main :: IO ()
main = do
  db <- newHandle "./data"
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> showMessages db
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

showMessages :: Handle -> IO ()
showMessages db = do
  mMessages <-
    runStore db $ do
      gMessages   <- openGlobalObject gMessagesId
      mMessageSet <- readObject gMessages
      case mMessageSet of
        Nothing -> do
          liftIO $ putStrLn "!!! messages collection doesn't exist !!!"
          pure []
        Just messageSet -> do
          messageRefs <- sequence $ ORSet.toList messageSet
          for messageRefs readObject
  pPrint $ sortOn postTime $ catMaybes mMessages
