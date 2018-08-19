{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           RON.Internal.Prelude

import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (for_)
import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import           Data.String.Interpolate.IsString (i)
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
import qualified RON.Binary as RB
import qualified RON.Binary.Parse as RB
import qualified RON.Binary.Serialize as RB
import           RON.Data.LWW (LWW (..), lwwType)
import           RON.Data.RGA (RGA (..), RgaText (..))
import           RON.Event (CalendarEvent (..), EpochEvent (..),
                            Naming (ApplicationSpecific, TrieForked),
                            ReplicaId (..), decodeEvent, encodeEvent,
                            fromCalendarEvent, mkCalendarDateTime)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import qualified RON.Text as RT
import qualified RON.Text.Parse as RT
import qualified RON.Text.Serialize as RT
import           RON.Typed (Object (..), Replicated, View, fromStateChunk,
                            fromStateOps, initialize, initializeObject,
                            toStateChunk, toStateOps, view)
import           RON.Typed.LwwStruct (Field (..))
import qualified RON.Typed.LwwStruct as LwwStruct
import qualified RON.Typed.Text as TypedText
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
        (Right . RB.serializeString)
        RB.parseString

prop_binary_roundtrip_atom =
    -- TODO increase limits
    binaryRoundtrip (Gen.atom 1000) RB.serializeAtom RB.parseAtom

prop_binary_roundtrip_frame =
    -- TODO increase limits
    binaryRoundtrip (Gen.frame 10) RB.serialize RB.parse

textRoundtrip
    :: (Eq a, Show a)
    => Gen a
    -> (a -> ByteStringL)
    -> (ByteStringL -> Either String a)
    -> Property
textRoundtrip gen serialize parse = property $ do
    x <- forAll gen
    let bytes = serialize x
    annotate $ BSL.unpack bytes
    annotateShow bytes
    x' <- evalEitherS $ parse bytes
    x === x'

prop_text_roundtrip_uuid =
    textRoundtrip Gen.uuid RT.serializeUuid RT.parseUuid

prop_text_roundtrip_string =
    textRoundtrip
        (Gen.text (Range.exponential 0 10000) Gen.unicode)
        RT.serializeString
        RT.parseString

prop_text_roundtrip_atom =
    textRoundtrip (Gen.atom 10000) RT.serializeAtom RT.parseAtom

prop_text_roundtrip_op =
    -- TODO increase limits
    textRoundtrip (Gen.op 100) RT.serializeOp RT.parseOp

prop_text_roundtrip_frame =
    -- TODO increase limits
    textRoundtrip (Gen.frame 10) RT.serializeFrame RT.parseFrame

prop_text_roundtrip_frames =
    -- TODO increase limits
    textRoundtrip (Gen.frames 10) RT.serializeFrames RT.parseFrames

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
        LwwStruct.toStateOps
            "TestStruct" [Field "int" ts_int, Field "text" ts_text] this

    toStateChunk this TestStruct{ts_int, ts_text} =
        LwwStruct.toStateChunk
            "TestStruct" [Field "int" ts_int, Field "text" ts_text] this

    fromStateOps this _ body = do
        ts_int  <- LwwStruct.fromStateOps "int"  this body
        ts_text <- LwwStruct.fromStateOps "text" this body
        pure TestStruct{..}

    fromStateChunk header _ body = do
        ts_int  <- LwwStruct.fromStateChunk "int"  header body
        ts_text <- LwwStruct.fromStateChunk "text" header body
        pure TestStruct{..}

testStruct0 = TestStructView{tsv_int = 275, tsv_text = "275"}

testStruct1 = object 0x9f $ TestStruct
    { ts_int = object 0x177 $ LWW (event 567) 275
    , ts_text = object 0x2b7 $ RgaText $ RGA
        [(event 733, Just '2'), (event 734, Just '7'), (event 735, Just '5')]
    }
  where
    event t = EpochEvent (ls60 t) replica
    object o = Object (UUID (0xb000000000000000 + o) 0x2d83d30067100000)

replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

testStruct2 = [i|
    *lww #B/000000002V+r3pl1c4 @B/000000002V+r3pl1c4 :0
        'TestStruct' 'int' >B/000000005s+r3pl1c4 'text' >B/00000000As+r3pl1c4 !
    *lww #B/000000005s+r3pl1c4 @B/000000008s+r3pl1c4 :0 =275
    *rga #B/00000000As+r3pl1c4 @B/00000000BT+r3pl1c4 :0 '2'
    *rga #B/00000000As+r3pl1c4 @B/00000000BU+r3pl1c4 :0 '7'
    *rga #B/00000000As+r3pl1c4 @B/00000000BV+r3pl1c4 :0 '5'
    .
    |]

prop_lwwStruct = property $ do
    ts1 <- evalEitherS $ runNetworkSim $ runReplicaSim replica $
        initializeObject @TestStruct testStruct0
    testStruct1 === ts1
    ts2 <- evalEitherS $ TypedText.serialize ts1
    BSL.words testStruct2 === BSL.words ts2
    ts3 <- evalEitherS $ TypedText.parse ts2
    ts1 === ts3
