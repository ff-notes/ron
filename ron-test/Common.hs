{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Lazy as BSL
import           Data.List.Extra (dropEnd, isSuffixOf)
import           Data.Traversable (for)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified RON.Text as RT

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
        pure $ testProperty name $ property $ do
            frameIn  <- evalEitherS $ RT.parseFrame bytesIn
            frameOut <- evalEitherS $ RT.parseFrame bytesOut
            frameIn === frameOut
  where
    commonTestDir = "../gritzko~ron-test"

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
