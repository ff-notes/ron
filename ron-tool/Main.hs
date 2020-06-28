import           Options.Applicative (InfoMod, Parser, ParserInfo,
                                      ParserPrefs (..), customExecParser,
                                      defaultPrefs, fullDesc, helper, info,
                                      progDesc, (<**>))

main :: IO ()
main = do
  parseOptions >>= print

data Options = Options
  deriving (Show)

parseOptions :: IO Options
parseOptions =
  customExecParser prefs parserInfo
  where
    parserInfo = i parser "RON tool"

    parser = do
      pure Options{}

prefs :: ParserPrefs
prefs =
  defaultPrefs
    { prefDisambiguate = True
    , prefHelpLongEquals = True
    , prefMultiSuffix = "..."
    , prefShowHelpOnError = True
    }

i :: Parser a -> String -> ParserInfo a
i prsr desc = i_ prsr desc mempty

i_ :: Parser a -> String -> InfoMod a -> ParserInfo a
i_ prsr desc m = info (prsr <**> helper) $ fullDesc <> progDesc desc <> m
