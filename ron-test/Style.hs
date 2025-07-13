{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import RON.Prelude

import Control.Monad.Extra (allM)
import Data.Foldable (sum)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (
    doesDirectoryExist,
    listDirectory,
    withCurrentDirectory,
 )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, putStrLn, stderr)
import System.Process.Typed (
    byteStringInput,
    proc,
    readProcessStdout,
    readProcessStdout_,
    runProcess_,
    setStdin,
 )

minimumStylishness :: Double
minimumStylishness = 0.3

main :: IO ()
main =
    withCurrentDirectory projectDir do
        haskellFiles <- collectHaskellFiles
        styleOkCount <- sum . map fromEnum <$> for haskellFiles checkStyle
        let stylishness =
                fromIntegral styleOkCount / genericLength haskellFiles :: Double
        putStrLn $ "Stylishness: " <> show stylishness
        when (stylishness < minimumStylishness) $
            fail "Bad style"

isHaskellFile :: FilePath -> Bool
isHaskellFile path = takeExtension path == ".hs"

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
    entities <- listDirectory dir
    fold <$> for entities \case
        '.' : _ ->
            -- skip hidden
            pure []
        entity -> do
            let path = dir </> entity
            entityIsDirectory <- doesDirectoryExist path
            if entityIsDirectory then listFilesRecursive path else pure [path]

collectHaskellFiles :: IO [FilePath]
collectHaskellFiles =
    filter isHaskellFile . map (drop 2 {- strip "./" -})
        <$> listFilesRecursive "."

projectDir :: FilePath
projectDir = ".."

runFormatter :: FilePath -> IO ByteStringL
runFormatter file = readProcessStdout_ $ proc "fourmolu" ["--", file]

runColorDiff :: FilePath -> ByteStringL -> IO ExitCode
runColorDiff file content = do
    (diffResult, plainDiff) <-
        readProcessStdout $
            proc "diff" ["-u", "--", file, "-"]
                & setStdin (byteStringInput content)
    case diffResult of
        ExitFailure 1 ->
            runProcess_ $
                proc "colordiff" [] & setStdin (byteStringInput plainDiff)
        _ -> pure ()
    pure diffResult

checkFormatting :: FilePath -> IO Bool
checkFormatting file = do
    formatted <- runFormatter file
    diffResult <- runColorDiff file formatted
    pure $ case diffResult of
        ExitSuccess -> True
        ExitFailure 1 -> False
        ExitFailure _ -> error "internal error: diff failed"

checkStyle :: FilePath -> IO Bool
checkStyle file = allM ($ file) [checkFormatting, checkWidth]

checkWidth :: FilePath -> IO Bool
checkWidth file = do
    content <- Text.readFile file
    and <$> for (zip [1 :: Int ..] $ Text.lines content) \(n, line) ->
        if Text.length line > 80
            then do
                hPutStrLn stderr $
                    intercalate ":" [file, show n, "line is too long"]
                pure False
            else pure True
