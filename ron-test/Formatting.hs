import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (Sum (Sum))
import RON.Prelude
import RON.Util (ByteStringL)
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
    withCurrentDirectory
    )
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>), takeExtension)
import System.Process.Typed
  ( ExitCodeException (..),
    byteStringInput,
    proc,
    readProcessStdout_,
    setStdin
    )

-- TODO 80
main :: IO ()
main =
  withCurrentDirectory projectDir $ do
    haskellFiles <- collectHaskellFiles
    Sum styleFails <-
      execWriterT . for haskellFiles $ \file -> do
        formatted <- runOrmolu file
        diff <- runColorDiff file formatted
        unless (BSL.null diff) count
    let styleFailShare =
          fromIntegral styleFails / genericLength haskellFiles :: Double
    when (styleFailShare > 1)
      $ fail "Bad style"

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

runOrmolu :: MonadIO io => FilePath -> io ByteStringL
runOrmolu file = readProcessStdout_ $ proc "ormolu" ["--", file]

runColorDiff :: MonadIO io => FilePath -> ByteStringL -> io ByteStringL
runColorDiff file content = do
  plainDiff <-
    liftIO $ readProcessStdout_
      (setStdin (byteStringInput content) $ proc "diff" ["-u", "--", file, "-"])
      `catch` \case
        ExitCodeException {eceExitCode = ExitFailure 1} -> pure BSL.empty
        ece -> throwIO ece
  if BSL.null plainDiff
    then pure BSL.empty
    else
      readProcessStdout_
        $ setStdin (byteStringInput plainDiff)
        $ proc "colordiff" []
