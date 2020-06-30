{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable (fold, traverse_)
import           Options.Applicative (InfoMod, Parser, ParserInfo,
                                      ParserPrefs (..), command,
                                      customExecParser, defaultPrefs, fullDesc,
                                      help, helper, info, long, metavar,
                                      progDesc, strOption, subparser, (<**>))

import           RON.Store (listObjects)
import           RON.Store.FS (debugDump, newHandle, runStore)
import           RON.Text (uuidToString)

main :: IO ()
main = do
  Options{..} <- parseOptions
  db <- newHandle dbPath
  case cmd of
    Dump -> debugDump dbPath
    List -> runStore db listObjects >>= traverse_ (putStrLn . uuidToString)

data Options = Options
  { dbPath :: FilePath
  , cmd    :: Command
  }
  deriving (Show)

data Command = Dump | List
  deriving (Show)

parseOptions :: IO Options
parseOptions =
  customExecParser prefs parserInfo
  where
    parserInfo = i parser "RON tool"

    parser = do
      dbPath <-
        strOption $ long "db" <> metavar "PATH" <> help "path to database"
      cmd <- subparser commands
      pure Options{..}

    commands =
      fold
        [ command "dump" $ i (pure Dump) "dump objects"
        , command "list" $ i (pure List) "list objects"
        ]

prefs :: ParserPrefs
prefs =
  defaultPrefs
    { prefDisambiguate    = True
    , prefHelpLongEquals  = True
    , prefMultiSuffix     = "..."
    , prefShowHelpOnError = True
    }

i :: Parser a -> String -> ParserInfo a
i prsr desc = i_ prsr desc mempty

i_ :: Parser a -> String -> InfoMod a -> ParserInfo a
i_ prsr desc m = info (prsr <**> helper) $ fullDesc <> progDesc desc <> m
