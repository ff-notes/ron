{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import           Internal.Prelude

import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Foldable (for_)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (Gen, MonadTest, Property, annotate, annotateShow,
                           forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range
import           System.Directory (getCurrentDirectory)
import           System.Environment (getEnv, lookupEnv, setEnv)
import           System.Info (os)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified RON.Base64 as Base64
import qualified RON.Binary as Binary
import qualified RON.Text as Text
import qualified RON.Text.Parse as Text
import qualified RON.Text.Serialize as Text
import           RON.Types (Op (..), UUID (..))
import qualified RON.UUID as UUID

import qualified Gen

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

prop_binary_roundtrip = property $ do
    frame <- forAll $ Gen.frame 1000
    let bytes = Binary.serialize frame
    annotateShow bytes
    Right frame === Binary.parse bytes

textRoundtrip
    :: (Eq a, Show a)
    => Gen a
    -> (a -> ByteStringL)
    -> (ByteStringL -> Either String a)
    -> Property
textRoundtrip gen serialize parse = property $ do
    x <- forAll gen
    let bytes = serialize x
    annotate $ BS.unpack bytes
    annotateShow bytes
    x' <- evalEitherS $ parse bytes
    x === x'

prop_text_roundtrip_uuid =
    textRoundtrip Gen.uuid Text.serializeUuid Text.parseUuid

prop_text_roundtrip_op = textRoundtrip Gen.op Text.serializeOp Text.parseOp

prop_text_roundtrip_frame =
    -- TODO increase limits
    textRoundtrip (Gen.frame 10) Text.serializeFrame Text.parseFrame

prop_text_roundtrip_frames =
    -- TODO increase limits
    textRoundtrip (Gen.frames 10 10) Text.serializeFrames Text.parseFrames

prop_base64_roundtrip = property $ do
    bytes <- forAll $ fromStrict <$> Gen.bytes (Range.exponential 0 1000)
    let text   = Base64.encode bytes
    let bytes' = Base64.decode text
    Just bytes === bytes'

prop_base64x60_roundtrip = property $ do
    w <- forAll Gen.word60
    Base64.decode60 (Base64.encode60 w) === Just w

prop_long_uuid = property $
    Right (UUID 0xa001083105187209 0x89669e8a6aaecb6e) ===
    Text.parseUuid "A0123456789 8abcdefghij"

prop_uuid_abbreviations = property $ do
    -- serialize
    "A/LED" === Text.serializeUuid aLed
    -- parse
    for_ encodings $ \e ->
        Right aLed === Text.parseUuid (BS.pack e)
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
    aLed = either error id $ Text.parseUuid "A/LED"

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

prop_ron_json_example = property $ Right output === Text.parseFrame input
  where
    input =
        "*lww #1TUAQ+gritzko @`   :bar!"
        -- "*lww #1TUAQ+gritzko @`   :bar !\n\
        -- \     #(R            @`   :foo !"
        -- "*lww #1TUAQ+gritzko @`   :bar = 1\n\
        -- \     #(R            @`   :foo > (Q"
    output =
        [ Op{ typ      = lww
            , object   = barEvent
            , event    = barEvent
            , location = bar
            }
        ]
    bar = UUID.mkNameUnsafe "bar"
    lww = UUID.mkNameUnsafe "lww"
    barEvent = UUID 0x5d78a680000000 0x2af6b78fafcc0000
    -- barEvent = UUID.mkCalendarEvent 0x5d78a680000000 0x2af6b78fafcc0000
    -- TODO: serializeTyped =
    -- {
    --     "foo": {
    --         "bar": 1
    --     }
    -- }
