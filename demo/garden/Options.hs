{-# LANGUAGE NoImplicitPrelude #-}

module Options (
  Command (..), NodeOptions (..), Options (..), Peer (..),
  parseOptions,
) where

import           RON.Prelude

import           Control.Applicative ((<**>))
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import           Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth,
                              parseAbsoluteURI)
import           Options.Applicative (Parser, ParserInfo, ParserPrefs, ReadM,
                                      argument, auto, command, customExecParser,
                                      defaultPrefs, eitherReader, flag,
                                      fullDesc, help, helper, info, long,
                                      metavar, option, prefDisambiguate,
                                      prefHelpLongEquals, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc, strOption,
                                      subparser, value)
import           RON.Text.Parse (parseUuid)
import           RON.Types (UUID)
import           Text.Read (readEither)
import qualified Text.Show

data Peer = Peer{host :: String, port :: Int}

instance Show Peer where
  show Peer{host, port} =
    show
      nullURI
        { uriScheme = "ws:"
        , uriAuthority =
            Just nullURIAuth{uriRegName = host, uriPort = ':' : show port}
        }

data Options = Options
  { database  :: FilePath
  , cmd       :: Command
  }

data Command
  = Show
  | Add UUID
  | RunNode NodeOptions
  | RunUI NodeOptions

data ListenHost = AnyAddress | Localhost

instance Show ListenHost where
  show = \case
    AnyAddress -> "::"
    Localhost  -> "::1"

data NodeOptions = NodeOptions
  { peers       :: [Peer]
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
    parserInfo = i parser "RON demo application -- growing trees"

parser :: Parser Options
parser = do
  database <-
    strOption
      (   long    "data"
      <>  metavar "FILE"
      <>  help    "database (default: ./ron-demo-garden.sqlite)"
      <>  value   "./ron-demo-garden.sqlite"
      )
  cmd <-
    subparser
      (   command "show"  (i pShow  "Offline: Show trees")
      <>  command "add"   (i pAdd   "Offline: Add a branch")
      <>  command "node"  (i pNode  "Start node without UI")
      <>  command "ui"    (i pUI    "Start UI with network node")
      )
  pure Options{database, cmd}

  where
    pShow = pure Show

    pAdd = Add <$> argument readUuid (metavar "UUID")

    pNode = RunNode <$> nodeOptions

    pUI = RunUI <$> nodeOptions

nodeOptions :: Parser NodeOptions
nodeOptions = do
  peers <-
    many $
    option
      readPeer
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

readPeer :: ReadM Peer
readPeer =
  eitherReader \str -> do
    uri <-
      maybe
        (Left
          "Peer must be set as 'ws://HOST:PORT' or 'ws://:PORT' for localhost")
        Right
        (parseAbsoluteURI str)
    let URI{uriScheme, uriAuthority, uriPath, uriQuery, uriFragment} = uri
    unless (uriScheme   == "ws:") $ Left "URI scheme must be 'ws'"
    unless (uriPath     == ""   ) $ Left "Path must be empty"
    unless (uriQuery    == ""   ) $ Left "Query must be empty"
    unless (uriFragment == ""   ) $ Left "Fragment must be empty"
    auth <- maybe (Left "Host or port must present") Right uriAuthority
    let URIAuth{uriUserInfo, uriRegName, uriPort} = auth
    unless  (uriUserInfo == "") $ Left "User info must be empty"
    when    (uriPort     == "") $ Left "Port must be non-empty"
    port <- readEither $ drop 1 {- port string starts with ':' -} uriPort
    pure Peer{host = uriRegName, port}

readUuid :: ReadM UUID
readUuid = eitherReader $ parseUuid . TextL.encodeUtf8 . TextL.pack
