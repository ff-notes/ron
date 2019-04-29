{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import           RON.Prelude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Generics (gcompare)
import           Data.List.Extra (dropEnd)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import           Hedgehog (Property, property, (===))
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit (assertFailure, testCase)

import           RON.Data (reduceWireFrame)
import qualified RON.Text as RT
import qualified RON.Text.Serialize as RT
import           RON.Types (ClosedOp (..), UUID,
                            WireChunk (Closed, Query, Value),
                            WireReducedChunk (..))
import qualified RON.UUID as UUID

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
        chunksIn0  <- readChunks $ commonTestDir </> fileIn
        chunksOut0 <- readChunks $ commonTestDir </> fileOut
        pure $ testGroup name $
            case collectLefts (chunksIn0, chunksOut0) of
                Left errs ->
                    [testCase part $ assertFailure err | (part, err) <- errs]
                Right (chunksIn1, chunksOut1) ->
                    [ testProperty (BSLC.unpack $ RT.serializeUuid obj) $
                        reduceAndCompareChunks chunksIn2 chunksOut2
                    | (obj, (chunksIn2, chunksOut2)) <-
                        Map.assocs $ zipDef [] [] chunksIn1 chunksOut1
                    , not $ "06" `isPrefixOf` name
                    ]
  where
    commonTestDir = "../gritzko~ron-test"
    readChunks file = do
        bytes <- BSL.readFile file
        let eFrame = RT.parseWireFrame bytes
        pure $ groupObjects . filter isRelevant <$> eFrame
    zipDef a b = Map.merge
        (Map.mapMissing $ const (,b))
        (Map.mapMissing $ const (a,))
        (Map.zipWithMatched $ const (,))

groupObjects :: [WireChunk] -> Map UUID [WireChunk]
groupObjects chunks =
    Map.fromListWith (++) [(chunkObject chunk, [chunk])| chunk <- chunks]

reduceAndCompareChunks :: [WireChunk] -> [WireChunk] -> Property
reduceAndCompareChunks chunksIn chunksOut =
    property $ ((===) `on` prepareChunks) chunksOut (reduceWireFrame chunksIn)

prepareChunks :: [WireChunk] -> [WireChunk]
prepareChunks = map sortChunkOps . sortBy gcompare

sortChunkOps :: WireChunk -> WireChunk
sortChunkOps chunk = case chunk of
    Value rc@WireReducedChunk{wrcBody} ->
        Value rc{wrcBody = sortBy gcompare wrcBody}
    _ -> chunk

chunkObject :: WireChunk -> UUID
chunkObject = objectId . \case
    Closed op                         -> op
    Value WireReducedChunk{wrcHeader} -> wrcHeader
    Query WireReducedChunk{wrcHeader} -> wrcHeader

isRelevant :: WireChunk -> Bool
isRelevant = \case
    Query _ -> False
    Value WireReducedChunk{wrcHeader = ClosedOp{reducerId}} ->
        reducerId /= $(UUID.liftName "~")
    _ -> True

collectLefts :: (Either a b, Either a b) -> Either [(String, a)] (b, b)
collectLefts = \case
    (Left a1,  Left a2 ) -> Left [("in",  a1), ("out", a2)]
    (Left a,   Right _ ) -> Left [("in",  a)]
    (Right _,  Left a  ) -> Left [("out", a)]
    (Right b1, Right b2) -> Right (b1, b2)
