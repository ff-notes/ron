module Options (Command (..), UIOptions (..), parseCommand) where

import           Control.Applicative (many, optional, (<**>))
import           Data.Text (Text)
import           Options.Applicative (Parser, ParserInfo, ParserPrefs, auto,
                                      command, customExecParser, defaultPrefs,
                                      fullDesc, help, helper, info, long,
                                      metavar, option, prefDisambiguate,
                                      prefHelpLongEquals, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc,
                                      strArgument, subparser)

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

parseCommand :: IO Command
parseCommand =
  customExecParser prefs parserInfo
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
