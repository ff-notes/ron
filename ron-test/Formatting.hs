import Data.Monoid (Sum (Sum))
import RON.Prelude
import System.Directory (doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.FilePath ((</>), takeExtension)

-- TODO ormolu | diff
-- TODO 80
main :: IO ()
main =
  withCurrentDirectory projectDir $ do
    haskellFiles <- collectHaskellFiles
    Sum n <-
      execWriterT . for haskellFiles $ \_file -> do
        let styleFail = True
        when styleFail count
    fail $ show n

isHaskellFile :: FilePath -> Bool
isHaskellFile path = takeExtension path == ".hs"

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
  entities <- listDirectory dir
  fmap fold . for entities $ \case
    '.' : _ ->
      -- skip hidden
      pure []
    entity -> do
      let path = dir </> entity
      entityIsDirectory <- doesDirectoryExist path
      if entityIsDirectory
        then listFilesRecursive path
        else pure [path]

collectHaskellFiles :: IO [FilePath]
collectHaskellFiles =
  filter isHaskellFile
    . map (drop 2) {- strip "./" -}
    <$> listFilesRecursive "."

projectDir :: FilePath
projectDir = ".."

count :: MonadWriter (Sum Int) m => m ()
count = tell (1 :: Sum Int)
