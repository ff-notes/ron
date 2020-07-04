{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<|>))
import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Data.Function ((&))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (InfoMod, Parser, ParserInfo,
                                      ParserPrefs (..), command,
                                      customExecParser, defaultPrefs, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, progDesc, short, strOption,
                                      subparser, (<**>))
import           System.Directory (makeAbsolute)

import           RON.Store (listObjects)
import           RON.Store.FS (debugDump, newHandle, runStore)
import           RON.Text (uuidToString, uuidToText)

main :: IO ()
main = do
  Options{..} <- parseOptions
  case cmd of
    Dump DumpRon  -> debugDump dbPath
    Dump DumpJson -> dumpDB dbPath >>= printJson
    Dump DumpYaml -> dumpDB dbPath >>= printYaml
    List -> do
      db <- newHandle dbPath
      objects <- runStore db listObjects
      for_ objects $ putStrLn . uuidToString

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

dumpDB :: FilePath -> IO Value
dumpDB dbPath = do
  dbPathAbs <- makeAbsolute dbPath
  db        <- newHandle dbPath
  objectIds <- runStore db listObjects
  pure $
    Json.object
      [ "database" .= dbPathAbs
      , "objects" .=
        Json.object [uuidToText objectId .= () | objectId <- objectIds]
      ]

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