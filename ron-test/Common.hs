{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           RON.Internal.Prelude

import qualified Data.ByteString.Lazy as BSL
import           Data.Generics (gcompare)
-- import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.List.Extra (dropEnd, isSuffixOf, sortOn)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           RON.Data (reduce)
import qualified RON.Text as RT
import           RON.Types (Chunk (Query, Raw, Value), RChunk (RChunk), UUID,
                            chunkBody, chunkHeader, opObject)

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
    when (take 2 name `notElem` ["05", "06"]) $ do
        let reduced = reduce frameIn
        -- when (take 2 name == "04") $
        --     ((===) `on` filter (not . BSL.null) . BSLC.lines)
        --         bytesOut
        --         (RT.serializeFrame reduced)
        ((===) `on` prepareFrame) frameOut reduced
  where
    prepareFrame =
        map sortChunkOps . sortOn chunkObject . filter (not . isQuery)
    sortChunkOps chunk = case chunk of
        Value rc@RChunk{chunkBody} ->
            Value rc{chunkBody = sortBy gcompare chunkBody}
        _ -> chunk

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

chunkObject :: Chunk -> UUID
chunkObject = opObject . \case
    Raw op                    -> op
    Value RChunk{chunkHeader} -> chunkHeader
    Query RChunk{chunkHeader} -> chunkHeader

isQuery :: Chunk -> Bool
isQuery = \case
    Query _ -> True
    _       -> False
