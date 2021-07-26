module Options (
  Command (..), NodeOptions (..), Options (..), UIOptions (..), parseOptions,
) where

import           Control.Applicative (many, (<**>))
import           Data.Text (Text)
import           Options.Applicative (Parser, ParserInfo, ParserPrefs, auto,
                                      command, customExecParser, defaultPrefs,
                                      flag, fullDesc, help, helper, info, long,
                                      metavar, option, prefDisambiguate,
                                      prefHelpLongEquals, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc,
                                      strArgument, strOption, subparser, value)

data Options = Options
  { dataDir :: FilePath
  , cmd     :: Command
  }

data Command
  = Show
  | Post Text Text
  | RunNode NodeOptions
  | RunUI UIOptions NodeOptions

newtype UIOptions = UIOptions
  { username :: Text
  }

data ListenHost = AnyAddress | Localhost

instance Show ListenHost where
  show = \case
    AnyAddress -> "::"
    Localhost  -> "::1"

data NodeOptions = NodeOptions
  { peers       :: [Int]
  , listenHost  :: ListenHost
  , listenPorts :: [Int]
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

parseOptions :: IO Options
parseOptions =
  customExecParser prefs parserInfo
  where
    parserInfo = i parser "RON demo chat application"

parser :: Parser Options
parser =
  do
    dataDir <-
      strOption
        (   long    "data"
        <>  metavar "FILE"
        <>  help    "database (default: ./ron-demo-chat.sqlite)"
        <>  value   "./ron-demo-chat.sqlite"
        )
    cmd <-
      subparser
        (   command "show"  (i pShow  "Offline: Show chat messages and exit")
        <>  command "post"  (i pPost  "Offline: Post messages to chat")
        <>  command "node"  (i pNode  "Start node without UI")
        <>  command "ui"    (i pUI    "Start UI with network node")
        )
    pure Options{dataDir, cmd}
  where
    pShow = pure Show

    pPost =
      Post <$> strArgument (metavar "NAME") <*> strArgument (metavar "TEXT")

    pNode = RunNode <$> nodeOptions

    pUI =
      RunUI
      <$> do
            username <- strArgument $ metavar "NAME"
            pure UIOptions{username}
      <*> nodeOptions

nodeOptions :: Parser NodeOptions
nodeOptions = do
  peers <-
    many $
    option
      auto
      (   long    "peer"
      <>  metavar "PORT"
      <>  help    "Connect to localhost peers using specifed ports"
      )
  listenPorts <-
    many $
    option
      auto
      (   long    "listen"
      <>  metavar "PORT"
      <>  help    "Run server on specified port and accept connections"
      )
  listenHost <-
    flag
      Localhost
      AnyAddress
      (   long "listen-any"
      <>  help "Run server on any IP interface (::), default is localhost"
      )
  pure NodeOptions{peers, listenPorts, listenHost}
