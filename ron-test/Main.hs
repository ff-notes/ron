{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           RON.Internal.Prelude

import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Foldable (for_)
import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
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
import qualified RON.Binary.Parse as Binary
import qualified RON.Binary.Serialize as Binary
import           RON.Data.LWW (LWW (..), lwwType)
import           RON.Data.RGA (RGA (..), RgaText (..))
import           RON.Event (CalendarEvent (..), EpochEvent (..),
                            Naming (TrieForked), ReplicaId (..), decodeEvent,
                            encodeEvent, fromCalendarEvent)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import qualified RON.Text as Text
import qualified RON.Text.Parse as Text
import qualified RON.Text.Serialize as Text
import           RON.Typed (Field (..), Object (..), Replicated, View,
                            initialize, initializeObject, lwwStructToStateChunk,
                            lwwStructToStateOps, toStateChunk, toStateOps, view)
import           RON.Types (Atom (..), Chunk (Raw), Op (..), UUID (..))
import qualified RON.UUID as UUID

import qualified Gen
import           HexDump (hexdump)

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
        (Right . Binary.serializeString)
        Binary.parseString

prop_binary_roundtrip_atom =
    -- TODO increase limits
    binaryRoundtrip (Gen.atom 1000) Binary.serializeAtom Binary.parseAtom

prop_binary_roundtrip_frame =
    -- TODO increase limits
    binaryRoundtrip (Gen.frame 10) Binary.serialize Binary.parse

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

prop_text_roundtrip_string =
    textRoundtrip
        (Gen.text (Range.exponential 0 10000) Gen.unicode)
        Text.serializeString
        Text.parseString

prop_text_roundtrip_atom =
    textRoundtrip (Gen.atom 10000) Text.serializeAtom Text.parseAtom

prop_text_roundtrip_op =
    -- TODO increase limits
    textRoundtrip (Gen.op 100) Text.serializeOp Text.parseOp

prop_text_roundtrip_frame =
    -- TODO increase limits
    textRoundtrip (Gen.frame 10) Text.serializeFrame Text.parseFrame

prop_text_roundtrip_frames =
    -- TODO increase limits
    textRoundtrip (Gen.frames 10) Text.serializeFrames Text.parseFrames

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

evalMaybeS :: (MonadTest m, HasCallStack) => Maybe a -> m a
evalMaybeS = \case
    Nothing -> withFrozenCallStack $ failWith Nothing ""
    Just a  -> pure a

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

prop_event_roundtrip = property $ do
    event  <- forAll Gen.event
    uuid   <- evalMaybeS $ encodeEvent event
    event' <- evalMaybeS $ decodeEvent uuid
    event === event'

prop_ron_json_example = let
    input =
        "*lww #1TUAQ+gritzko @`   :bar = 1  ;\n\
        \     #(R            @`   :foo > (Q ;"
    output =
        [ Raw Op
            { opType     = lwwType
            , opObject   = bar
            , opEvent    = bar
            , opLocation = barName
            , opPayload  = [AInteger 1]
            }
        , Raw Op
            { opType     = lwwType
            , opObject   = foo
            , opEvent    = foo
            , opLocation = fooName
            , opPayload  = [AUuid bar]
            }
        ]
    barName = fromJust $ UUID.mkName "bar"
    bar     = fromJust $ encodeEvent $ fromCalendarEvent
            $ CalendarEvent (read "2017-10-31 10:26:00") replicaGritzko
    fooName = fromJust $ UUID.mkName "foo"
    foo     = fromJust $ encodeEvent $ fromCalendarEvent
            $ CalendarEvent (read "2017-10-31 10:27:00") replicaGritzko
    gritzko = fromJust $ Base64.decode60 "gritzko"
    replicaGritzko = ReplicaId TrieForked gritzko
    in
    property $ do
        parsed <- evalEitherS $ Text.parseFrame input
        output === parsed

data TestStructView = TestStructView{tsv_int :: Int64, tsv_text :: Text}
    deriving (Eq, Show)

data TestStruct = TestStruct
    {ts_int :: Object (LWW Int64), ts_text :: Object RgaText}
    deriving (Eq, Show)

instance Replicated TestStruct where
    type View TestStruct = TestStructView

    initialize TestStructView{..} = do
        ts_int  <- initializeObject tsv_int
        ts_text <- initializeObject tsv_text
        pure TestStruct{..}

    view TestStruct{..} = TestStructView
        { tsv_int  = view $ objectValue ts_int
        , tsv_text = view $ objectValue ts_text
        }

    toStateOps this TestStruct{ts_int, ts_text} =
        lwwStructToStateOps
            "TestStruct" [Field "int" ts_int, Field "text" ts_text] this

    toStateChunk this TestStruct{ts_int, ts_text} =
        lwwStructToStateChunk
            "TestStruct" [Field "int" ts_int, Field "text" ts_text] this

testStructInitial = TestStructView{tsv_int = 275, tsv_text = "275"}

testStruct1 = TestStruct
    { ts_int  = Object (UUID 0x83e96ff433c6dd65 0x20000000000000fa) $ LWW
        (EpochEvent (ls60 0xfc2ea3b9d703b09) $ ReplicaId TrieForked $ ls60 250)
        275
    , ts_text = Object (UUID 0x8fc2ea3b9d703b0a 0x20000000000000fa) $ RgaText $
        RGA
            [   ( EpochEvent
                    (ls60 0xfc2ea3b9d703b0b) (ReplicaId TrieForked (ls60 250))
                , Just '2'
                )
            ,   ( EpochEvent
                    (ls60 0xfc2ea3b9d703b0c) (ReplicaId TrieForked (ls60 250))
                , Just '7'
                )
            ,   ( EpochEvent
                    (ls60 0xfc2ea3b9d703b0d) (ReplicaId TrieForked (ls60 250))
                , Just '5'
                )
            ]
    }

prop_TestStruct_serialize = property $ do
    ts <- evalEitherS $ runNetworkSim $
        runReplicaSim (ReplicaId TrieForked $ ls60 250) $
            initialize @TestStruct testStructInitial
    testStruct1 === ts
