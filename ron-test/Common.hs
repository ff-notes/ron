{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Function (on)
import           Data.List.Extra (dropEnd, isSuffixOf, sortOn)
import           Data.Traversable (for)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           RON.Data (reduce)
import qualified RON.Text as RT
import           RON.Types (Chunk (Query, Raw, Value),
                            ReducedChunk (ReducedChunk), UUID, chunkHeader,
                            opObject)

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
    when (take 2 name `elem` ["01", "02", "03"]) $ do
        let reduced = reduce frameIn
        -- when (take 2 name == "03") $
        --     ((===) `on` filter (not . BSL.null) . BSLC.lines)
        --         bytesOut
        --         (RT.serializeFrame reduced)
        ((===) `on` sortOn chunkObject) frameOut reduced

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

chunkObject :: Chunk -> UUID
chunkObject = opObject . \case
    Raw op                          -> op
    Value ReducedChunk{chunkHeader} -> chunkHeader
    Query ReducedChunk{chunkHeader} -> chunkHeader
