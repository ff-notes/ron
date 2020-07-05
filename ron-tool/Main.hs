{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<|>))
import           Data.Aeson (ToJSON, Value, toJSON, (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as TextL
import           Data.Traversable (for)
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (Parser, ParserInfo, ParserPrefs (..),
                                      command, customExecParser, defaultPrefs,
                                      flag', fullDesc, help, helper, info, long,
                                      metavar, progDesc, short, strOption,
                                      subparser, (<**>))
import           System.Directory (makeAbsolute)

import           RON.Data.VersionVector (VV (..), serializeVV)
import           RON.Store (getObjectVersion, listObjects)
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
i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc

dumpDB :: FilePath -> IO Value
dumpDB dbPath = do
  dbPathAbs <- makeAbsolute dbPath
  db        <- newHandle dbPath
  objects   <- runStore db $ do
    objectIds <- listObjects
    for objectIds $ \objectId -> do
      version <- getObjectVersion objectId
      pure (objectId, version)
  pure $
    Json.object
      [ "database" .= dbPathAbs
      , "objects" .=
        Json.object
          [ uuidToText objectId .=
            Json.object
              [ "version"     .= VVJson version
              , "version.ron" .= TextL.decodeUtf8 (serializeVV version)
              ]
          | (objectId, version) <- objects
          ]
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

newtype VVJson = VVJson VV

instance ToJSON VVJson where
  toJSON (VVJson (VV m)) =
    Json.object
      [ Text.pack (show replica) .= Text.pack (show time)
      | (replica, time) <- Map.assocs m
      ]
