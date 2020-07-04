{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<|>))
import           Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (traverse_)
import           Data.Function ((&))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (InfoMod, Parser, ParserInfo,
                                      ParserPrefs (..), command,
                                      customExecParser, defaultPrefs, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, progDesc, short, strOption,
                                      subparser, (<**>))

import           RON.Store (listObjects)
import           RON.Store.FS (Handle, debugDump, newHandle, runStore)
import           RON.Text (uuidToString, uuidToText)

main :: IO ()
main = do
  Options{..} <- parseOptions
  db <- newHandle dbPath
  case cmd of
    Dump DumpRon  -> debugDump dbPath
    Dump DumpJson -> dump db >>= printJson
    Dump DumpYaml -> dump db >>= printYaml
    List -> runStore db listObjects >>= traverse_ (putStrLn . uuidToString)

data Options = Options
  { dbPath :: FilePath
  , cmd    :: Command
  }
  deriving (Show)

data Command = Dump DumpFormat | List
  deriving (Show)

data DumpFormat = DumpRon | DumpJson | DumpYaml
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

    commands
      =   command "dump" (i (Dump <$> pDumpFormat) "dump objects")
      <>  command "list" (i (pure List)            "list objects")

    pDumpFormat
      =   flag' DumpJson (short 'j' <> long "json")
      <|> flag' DumpRon  (short 'r' <> long "ron")
      <|> flag' DumpYaml (short 'y' <> long "yaml")

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

dump :: Handle -> IO Value
dump db = do
  objectIds <- runStore db listObjects
  pure $ object [uuidToText objectId .= () | objectId <- objectIds]

printJson :: Value -> IO ()
printJson =
  BSL.putStr . Json.encodePretty' config
  where
    config =
      Json.defConfig
        {Json.confCompare = compare, Json.confTrailingNewline = True}

printYaml :: Value -> IO ()
printYaml =
  BS.putStr . Yaml.encodePretty config
  where
    config = Yaml.defConfig & Yaml.setConfCompare compare
