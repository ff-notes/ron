{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           RON.Internal.Prelude

import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char (toLower, toUpper)
import           Data.Foldable (for_)
import           Data.Maybe (fromJust)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (Gen, MonadTest, Property, PropertyT, annotate,
                           annotateShow, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range
import           System.Directory (getCurrentDirectory)
import           System.Environment (getEnv, lookupEnv, setEnv)
import           System.Info (os)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified RON.Base64 as Base64
import qualified RON.Binary as RB
import qualified RON.Binary.Parse as RB
import qualified RON.Binary.Serialize as RB
import           RON.Event (CalendarEvent (..), Naming (TrieForked),
                            ReplicaId (..), decodeEvent, encodeEvent,
                            fromCalendarEvent, mkCalendarDateTime)
import qualified RON.Text as RT
import qualified RON.Text.Parse as RT
import qualified RON.Text.Serialize as RT
import           RON.Types (Atom (..), Chunk (Raw), Op (..), Op' (..),
                            UUID (..))
import qualified RON.UUID as UUID

import qualified Gen
import           HexDump (hexdump)
import qualified LwwStruct

main :: IO ()
main = do
    prepareEnv
    $defaultMainGenerator

prepareEnv :: IO ()
prepareEnv = do
    curDir <- getCurrentDirectory
    let buildDir = curDir ++ "/../../.build"

    do  path <- getEnv "PATH"
        setEnv "PATH" $ buildDir ++ ":" ++ path

    do  mLibraryPath <- lookupEnv libraryPathVar
        let libraryPath = case mLibraryPath of
                Nothing -> buildDir
                Just lp -> buildDir ++ ":" ++ lp
        setEnv libraryPathVar libraryPath

  where
    libraryPathVar = case os of
        "darwin" -> "DYLD_LIBRARY_PATH"
        "linux"  -> "LD_LIBRARY_PATH"
        _ -> error $ os ++ " is not supported"

binaryRoundtrip
    :: (Eq a, Show a)
    => Gen a
    -> (a -> Either String ByteStringL)
    -> (ByteStringL -> Either String a)
    -> Property
binaryRoundtrip gen serialize parse = property $ do
    x <- forAll gen
    bytes <- evalEitherS $ serialize x
    annotate $ hexdump bytes
    x' <- evalEitherS $ parse bytes
    x === x'

prop_binary_roundtrip_string =
    binaryRoundtrip
        (Gen.text (Range.exponential 0 1000) Gen.unicode)
        (Right . RB.serializeString)
        RB.parseString

prop_binary_roundtrip_atom =
    -- TODO increase limits
    binaryRoundtrip (Gen.atom 1000) RB.serializeAtom RB.parseAtom

prop_binary_roundtrip_frame =
    -- TODO increase limits
    binaryRoundtrip (Gen.frame 10) RB.serialize RB.parse

textRoundtrip
    :: (Eq a, Show a, Monad m)
    => Gen a
    -> (a -> ByteStringL)
    -> (ByteStringL -> Either String a)
    -> PropertyT m ()
textRoundtrip gen serialize parse = do
    x <- forAll gen
    let bytes = serialize x
    annotate $ BSL.unpack bytes
    annotateShow bytes
    x' <- evalEitherS $ parse bytes
    x === x'

prop_text_roundtrip_uuid =
    property $ textRoundtrip Gen.uuid RT.serializeUuid RT.parseUuid

prop_text_roundtrip_string = property $
    textRoundtrip
        (Gen.text (Range.exponential 0 10000) Gen.unicode)
        RT.serializeString
        RT.parseString

prop_text_roundtrip_atom =
    property $ textRoundtrip (Gen.atom 10000) RT.serializeAtom RT.parseAtom

prop_text_roundtrip_op =
    -- TODO increase limits
    property $ textRoundtrip (Gen.op 100) RT.serializeOp RT.parseOp

prop_text_roundtrip_frame =
    -- TODO increase limits
    property $ textRoundtrip (Gen.frame 10) RT.serializeFrame RT.parseFrame

prop_text_roundtrip_frames =
    -- TODO increase limits
    property $ textRoundtrip (Gen.frames 10) RT.serializeFrames RT.parseFrames

prop_filename_roundtrip = property $ do
    ShowAs caseTransform _ <- forAll $ Gen.element
        [ id          `ShowAs` "id"
        , map toUpper `ShowAs` "map toUpper"
        , map toLower `ShowAs` "map toLower"
        ]
    textRoundtrip
        Gen.uuid
        (BSL.pack . caseTransform . UUID.encodeBase32)
        (maybe (Left "Filename decoding error") Right .
            UUID.decodeBase32 . BSL.unpack)

prop_word64base32_roundtrip = property $
    textRoundtrip
        Gen.word64'
        (fromStrict . Base64.encode64base32short)
        (maybe (Left "Decoding error") Right .
            Base64.decode64base32 . BSL.toStrict)

prop_base64_roundtrip = property $ do
    bytes <- forAll $ fromStrict <$> Gen.bytes (Range.exponential 0 1000)
    let text   = Base64.encode bytes
    let bytes' = Base64.decode text
    Just bytes === bytes'

prop_word60base64_roundtrip = property $ do
    w <- forAll Gen.word60
    Base64.decode60 (Base64.encode60 w) === Just w

prop_long_uuid = property $
    Right (UUID 0xa001083105187209 0x89669e8a6aaecb6e) ===
    RT.parseUuid "A0123456789 8abcdefghij"

prop_uuid_abbreviations = property $ do
    -- serialize
    "A/LED" === RT.serializeUuid aLed
    -- parse
    for_ encodings $ \e -> do
        annotateShow e
        Right aLed === RT.parseUuid (BSL.pack e)
  where
    encodings =
        [ "ALED0000000 00000000000"
        , "ALED000000000000000000"
        , "ALED0000000 0000000000"
        , "ALED0000000$0000000000"
        , "ALED0000000"
        , "A/LED000 0"
        , "A/LED$0"
        , "A/LED"
        ]
    aLed = either error id $ RT.parseUuid "A/LED"

-- evalMaybeS :: (MonadTest m, HasCallStack) => Maybe a -> m a
-- evalMaybeS = \case
--     Nothing -> withFrozenCallStack $ failWith Nothing ""
--     Just a  -> pure a

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

prop_event_roundtrip = property $ do
    event  <- forAll Gen.event
    let uuid   = encodeEvent event
    let event' = decodeEvent uuid
    event === event'

prop_ron_json_example = let
    input =
        "*lww #1TUAQ+gritzko @`   :bar = 1  ;\n\
        \     #(R            @`   :foo > (Q ;"
    output =
        [ Raw Op
            { opType = lwwType
            , opObject = bar
            , op' =
                Op'{opEvent = bar, opRef = barName, opPayload = [AInteger 1]}
            }
        , Raw Op
            { opType = lwwType
            , opObject = foo
            , op' = Op'{opEvent = foo, opRef = fooName, opPayload = [AUuid bar]}
            }
        ]
    barName = fromJust $ UUID.mkName "bar"
    bar     = encodeEvent $ fromCalendarEvent $ CalendarEvent
                (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 26, 00))
                replicaGritzko
    fooName = fromJust $ UUID.mkName "foo"
    foo     = encodeEvent $ fromCalendarEvent $ CalendarEvent
                (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 27, 00))
                replicaGritzko
    gritzko = fromJust $ Base64.decode60 "gritzko"
    replicaGritzko = ReplicaId TrieForked gritzko
    in
    property $ do
        parsed <- evalEitherS $ RT.parseFrame input
        output === parsed

lwwType = fromJust $ UUID.mkName "lww"

prop_lwwStruct = LwwStruct.prop_lwwStruct

data ShowAs a = ShowAs a String

instance Show (ShowAs a) where
    show (ShowAs _ s) = s
