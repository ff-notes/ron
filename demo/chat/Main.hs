import           Prelude hiding (show)
import           RON.Prelude (show)

import           Control.Applicative (many, optional, (<**>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Generics.Labels ()
import           Data.List (sortOn)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Traversable (for)
import           Options.Applicative (Parser, ParserInfo, ParserPrefs, auto,
                                      command, customExecParser, defaultPrefs,
                                      fullDesc, help, helper, info, long,
                                      metavar, option, prefDisambiguate,
                                      prefHelpLongEquals, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc,
                                      strArgument, subparser)
import           RON.Data.ORSet.Experimental (ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Store (MonadStore, newObject, openNamedObject, readObject)
import           RON.Store.FS (Handle, newHandle, runStore)
import           RON.Types (Atom (AString), ObjectRef)
import           Text.Pretty.Simple (pPrint)

import           Types (Message, postTime)

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  Text -> Text -> m (ObjectRef Message)
newMessage username text = do
  gMessages <- openMessages
  msgRef <- newObject @Message
  ORSet.add_ msgRef ("username", [AString username])
  ORSet.add_ msgRef ("text",     [AString text    ])
  ORSet.add_ gMessages msgRef
  pure msgRef

openMessages ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  m (ObjectRef (ORSet (ObjectRef Message)))
openMessages = openNamedObject "messages"

main :: IO ()
main =
  do
    cmd <- parseCommand
    db <- newHandle "./data"
    case cmd of
      Show -> showMessages db
      Post username text -> do
        messageRef <- runStore db $ newMessage username text
        putStrLn $ "created message: " <> show messageRef
      UI _username -> undefined

parseCommand :: IO Command
parseCommand = customExecParser prefs parserInfo
  where
    parserInfo = i parser "RON demo chat application"

parser :: Parser Command
parser =
  subparser
    $   command "show" (i pShow "Show chat messages and exit (offline)")
    <>  command "post" (i pPost "Post messages to chat (offline)")
    <>  command "ui"   (i pUI   "Start UI with network")
  where
    pShow = pure Show

    pPost =
      Post <$> strArgument (metavar "NAME") <*> strArgument (metavar "TEXT")

    pUI = do
      username <- strArgument $ metavar "NAME"
      peers    <-
        many $
        option auto
          $   long    "peer"
          <>  metavar "PORT"
          <>  help    "Connect to localhost peers using specifed ports"
      listen <-
        optional $
        option auto
          $   long    "listen"
          <>  metavar "PORT"
          <>  help    "Run server on specified port and accept connections"
      pure $ UI UIOptions{..}

showMessages :: Handle -> IO ()
showMessages db = do
  mMessages <-
    runStore db $ do
      gMessages   <- openMessages
      mMessageSet <- readObject gMessages
      case mMessageSet of
        Nothing -> do
          liftIO $ putStrLn "!!! messages collection doesn't exist !!!"
          pure []
        Just messageSet -> do
          messageRefs <- ORSet.toList messageSet
          for messageRefs readObject
  pPrint $ sortOn postTime $ catMaybes mMessages

data Command = Show | UI UIOptions | Post Text Text

data UIOptions = UIOptions
  { username :: Text
  , peers    :: [Int]
  , listen   :: Maybe Int
  }

prefs :: ParserPrefs
prefs =
  defaultPrefs
    { prefDisambiguate    = True
    , prefHelpLongEquals  = True
    , prefMultiSuffix     = "..."
    , prefShowHelpOnError = True
    }

i :: Parser a -> String -> ParserInfo a
i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc
