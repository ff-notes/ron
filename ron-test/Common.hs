{-# OPTIONS -Wno-missing-signatures #-}

import qualified Data.ByteString.Lazy as BSL
import           Data.List.Extra (dropEnd, isSuffixOf)
import           Data.Traversable (for)
import           Hedgehog (property, (===))
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
    cases <- loadCases
    defaultMain $ testGroup "Common" cases

loadCases :: IO [TestTree]
loadCases = do
    files <- filter (".in.ron" `isSuffixOf`) <$> listDirectory commonTestDir
    for files $ \fileIn -> do
        let name = dropEnd 7 fileIn
            fileOut = name <> ".out.ron"
        bytesIn  <- BSL.readFile $ commonTestDir </> fileIn
        bytesOut <- BSL.readFile $ commonTestDir </> fileOut
        pure $ testProperty name $ property $ bytesIn === bytesOut
  where
    commonTestDir = "../gritzko~ron-test"
