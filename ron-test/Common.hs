{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Lazy as BSL
import           Data.List.Extra (dropEnd, isSuffixOf)
import           Data.Traversable (for)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified RON.Text as RT

type ByteStringL = BSL.ByteString

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
        pure $ test name bytesIn bytesOut
  where
    commonTestDir = "../gritzko~ron-test"

test :: TestName -> ByteStringL -> ByteStringL -> TestTree
test name bytesIn bytesOut = testProperty name $ property $ do
    frameIn  <- evalEitherS $ RT.parseFrame bytesIn
    frameOut <- evalEitherS $ RT.parseFrame bytesOut
    frameIn === frameOut

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
