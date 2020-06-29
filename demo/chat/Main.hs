{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           RON.Data.Experimental (Rep, ReplicatedObject, view)
import           RON.Data.ORSet.Experimental (ORSetRep)
import qualified RON.Data.ORSet.Experimental as ORSet
import qualified RON.Epoch as Epoch
import           RON.Error (MonadE, errorContext, liftMaybe, throwErrorText)
import           RON.Event (ReplicaClock, decodeEvent)
import           RON.Store (MonadStore, newObject, openGlobalObject, readObject)
import           RON.Store.FS (Handle, newHandle, runStore)
import           RON.Types (Atom (AString, AUuid), ObjectRef (..), UUID)
import qualified RON.UUID as UUID

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
  (MonadE m, MonadStore m, ReplicaClock m) =>
  Text -> Text -> m (ObjectRef Message)
newMessage username text = do
  gMessages :: ObjectRef ORSetRep <- openGlobalObject gMessagesId
  msgRef@(ObjectRef msgId) <- newObject
  ORSet.add_ msgRef ["username", AString username]
  ORSet.add_ msgRef ["text",     AString text    ]
  ORSet.add_ gMessages [AUuid msgId]
  pure msgRef

lookupLwwText :: MonadE m => Atom -> ORSetRep -> m Text
lookupLwwText key orset = do
  payload <- liftMaybe ("lookup " <> show key) $ ORSet.lookupLww key orset
  case payload of
    [AString text] -> pure text
    _ ->
      throwErrorText $ "Value at " <> show key <> " is expected to be a string"

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
          let
            messageRefsEncoded = ORSet.toList messageSet
            messageRefs =
              [ObjectRef uuid | [AUuid uuid] <- messageRefsEncoded]
          for messageRefs readObject
  pPrint $ sortOn postTime $ catMaybes mMessages
