import           Prelude hiding (show)
import           RON.Prelude (show)

import           Control.Applicative (many, optional, (<**>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Generics.Labels ()
import           Data.List (sortOn)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Traversable (for)
import           Options.Applicative (InfoMod, Parser, ParserInfo, ParserPrefs,
                                      auto, customExecParser, defaultPrefs,
                                      fullDesc, helper, info, long, metavar,
                                      option, prefDisambiguate,
                                      prefHelpLongEquals, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc,
                                      strArgument)
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
    Options{cmd} <- customExecParser prefs parserInfo
    db <- newHandle "./data"
    case cmd of
      Show -> showMessages db
      Add username text -> do
        messageRef <- runStore db $ newMessage username text
        putStrLn $ "created message: " <> show messageRef
      UI _username -> undefined
  where
    parserInfo = i parser "RON demo chat application" mempty
    parser = do
      peers     <- many     $ option auto $ long "peer" <> metavar "PORT"
      port      <- optional $ option auto $ long "port" <> metavar "PORT"
      mUsername <- optional $ strArgument $ metavar "NAME"
      mText     <- optional $ strArgument $ metavar "TEXT"
      pure
        Options
          { cmd =
              case (mUsername, mText) of
                (Nothing,       Nothing)   -> Show
                (Just username, Just text) -> Add username text
                (Just username, Nothing)   -> UI  username
                (Nothing,       Just _)    -> error "internal"
          , ..
          }

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

data Options = Options
  { cmd   :: Command
  , peers :: [Int]
  , port  :: Maybe Int
  }

data Command = Show | UI Text | Add Text Text

prefs :: ParserPrefs
prefs =
  defaultPrefs
    { prefDisambiguate    = True
    , prefHelpLongEquals  = True
    , prefMultiSuffix     = "..."
    , prefShowHelpOnError = True
    }

i :: Parser a -> String -> InfoMod a -> ParserInfo a
i prsr desc m = info (prsr <**> helper) $ fullDesc <> progDesc desc <> m
