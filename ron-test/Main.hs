{-# OPTIONS -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import RON.Prelude

import Control.Lens ((+~))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as TextL
import Hedgehog (
    Gen,
    MonadTest,
    Property,
    PropertyT,
    annotate,
    annotateShow,
    evalExceptT,
    forAll,
    property,
    tripping,
    (===),
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv, lookupEnv, setEnv)
import System.Info (os)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (defaultMainGenerator)
import Text.Show qualified

import RON.Base64 qualified as Base64
import RON.Binary.Parse qualified as RB
import RON.Binary.Serialize qualified as RB
import RON.Data (
    evalObjectState,
    execObjectState,
    newObjectFrame,
    newObjectFrameWith,
    runObjectState,
 )
import RON.Data.ORSet (ORSet (ORSet))
import RON.Data.ORSet qualified as ORSet
import RON.Data.RGA (RgaString)
import RON.Data.RGA qualified as RGA
import RON.Event (
    OriginVariety (ApplicationSpecific, TrieForked),
    decodeEvent,
    encodeEvent,
    mkCalendarDateTime,
    mkCalendarEvent,
    mkReplica,
 )
import RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import RON.Text qualified as RT
import RON.Text.Parse qualified as RT
import RON.Text.Serialize qualified as RT
import RON.Text.Serialize.Experimental qualified as RT
import RON.Text.Serialize.UUID qualified as RT
import RON.Types (
    Atom (AInteger, AUuid),
    ClosedOp (ClosedOp, objectId, op, reducerId),
    ObjectFrame,
    Op (Op, opId, payload, refId),
    UUID (UUID),
    WireChunk (Closed),
 )
import RON.UUID qualified as UUID

import Gen qualified
import HexDump (hexdump)
import LwwStruct qualified
import ORSet qualified
import Orphans ()
import String (s)
import StructSet qualified
import Types (
    TestRecursiveORSet (TestRecursiveORSet),
    testRecSet,
    testRecSet_zoom,
 )

main :: IO ()
main = do
    prepareEnv
    $defaultMainGenerator

prepareEnv :: IO ()
prepareEnv = do
    curDir <- getCurrentDirectory
    let buildDir = curDir ++ "/../../.build"

    do
        path <- getEnv "PATH"
        setEnv "PATH" $ buildDir ++ ":" ++ path

    do
        mLibraryPath <- lookupEnv libraryPathVar
        let libraryPath = case mLibraryPath of
                Nothing -> buildDir
                Just lp -> buildDir ++ ":" ++ lp
        setEnv libraryPathVar libraryPath
  where
    libraryPathVar = case os of
        "darwin" -> "DYLD_LIBRARY_PATH"
        "linux" -> "LD_LIBRARY_PATH"
        _ -> error $ os ++ " is not supported"

binaryRoundtrip ::
    (Eq a, Show a) =>
    Gen a ->
    (a -> Either String ByteStringL) ->
    (ByteStringL -> Either String a) ->
    Property
binaryRoundtrip gen serialize parse = property do
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
    -- TODO(2018-11-08, cblp, #4) increase limits
    binaryRoundtrip (Gen.atom 1000) RB.serializeAtom RB.parseAtom

-- TODO(2018-10-02, cblp, #23) fix and enable
-- prop_binary_roundtrip_frame =
--     -- TODO(2018-11-08, cblp, #4) increase limits
--     binaryRoundtrip (Gen.wireFrame 10) RB.serialize RB.parse

textRoundtrip ::
    (Show a, Show b, Applicative f, Eq (f a), Show (f a), Monad m) =>
    Gen a ->
    (a -> b) ->
    (b -> f a) ->
    PropertyT m ()
textRoundtrip gen serialize parse = do
    x <- forAll gen
    tripping x serialize parse

prop_text_roundtrip_uuid =
    property $ textRoundtrip Gen.uuid RT.serializeUuid RT.parseUuid

prop_text_roundtrip_uuid_zip = property do
    prevKey <- forAll Gen.uuid
    prev <- forAll Gen.uuid
    textRoundtrip
        Gen.uuid
        (RT.serializeUuidKey prevKey prev)
        (RT.parseUuidKey prevKey prev)
    textRoundtrip Gen.uuid (RT.serializeUuidAtom prev) (RT.parseUuidAtom prev)

prop_text_roundtrip_string =
    property $
        textRoundtrip
            (Gen.text (Range.exponential 0 10_000) Gen.unicode)
            RT.serializeString
            RT.parseString

prop_text_roundtrip_atom =
    property $ textRoundtrip (Gen.atom 10_000) RT.serializeAtom RT.parseAtom

prop_text_roundtrip_op =
    -- TODO(2018-11-08, cblp, #4) increase limits
    property $ textRoundtrip (Gen.closedOp 100) RT.serializeRawOp RT.parseOp

prop_text_roundtrip_wireFrame =
    property $
        -- TODO(2018-11-08, cblp, #4) increase limits
        textRoundtrip (Gen.wireFrame 10) RT.serializeWireFrame RT.parseWireFrame

prop_text_roundtrip_stateFrame =
    property $
        -- TODO(2018-11-08, cblp, #4) increase limits
        textRoundtrip
            (Gen.stateFrame 10)
            RT.serializeStateFrame
            RT.parseStateFrame

prop_text_roundtrip_frames =
    property $
        -- TODO(2018-11-08, cblp, #4) increase limits
        textRoundtrip
            (Gen.wireFrames 10)
            RT.serializeWireFrames
            RT.parseWireFrames

prop_filename_roundtrip = property do
    ShowAs caseTransform _ <-
        forAll $
            Gen.element
                [ id `ShowAs` "id"
                , map toUpper `ShowAs` "map toUpper"
                , map toLower `ShowAs` "map toLower"
                ]
    textRoundtrip
        Gen.uuid
        (BSLC.pack . caseTransform . UUID.encodeBase32)
        ( note ("Filename decoding error" :: String)
            . UUID.decodeBase32
            . BSLC.unpack
        )

prop_word64base32_roundtrip =
    property $
        textRoundtrip
            Gen.word64'
            (fromStrict . Base64.encode64base32short)
            ( note ("Decoding error" :: String)
                . Base64.decode64base32
                . BSLC.toStrict
            )

prop_base64_roundtrip = property do
    bytes <- forAll $ fromStrict <$> Gen.bytes (Range.exponential 0 1000)
    tripping bytes Base64.encode Base64.decode

prop_word60base64_roundtrip = property do
    w <- forAll Gen.word60
    tripping w Base64.encode60 Base64.decode60

prop_long_uuid =
    property $
        Right (UUID 0x_a001_0831_0518_7209 0x_8966_9e8a_6aae_cb6e)
            === RT.parseUuid "A01234567898abcdefghij"

prop_uuid_abbreviations = property do
    -- serialize
    "A/LED" === RT.serializeUuid aLed
    -- parse
    for_ encodings \e -> do
        annotateShow e
        Right aLed === RT.parseUuid (BSLC.pack e)
  where
    encodings =
        [ "ALED000000000000000000"
        , "ALED0000000$0000000000"
        , "ALED0000000"
        , "A/LED$0"
        , "A/LED"
        ]
    aLed = either error id $ RT.parseUuid "A/LED"

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = evalExceptT . liftEither

prop_event_roundtrip = property do
    event <- forAll Gen.anyEvent
    tripping event encodeEvent decodeEvent

prop_name_roundtip = property do
    name <- forAll genName
    tripping name UUID.mkName \mu -> do
        u <- mu
        (n, "") <- UUID.getName u
        pure $ dropZeroesEnd n
    scope <- forAll genName
    tripping (scope, name) (uncurry UUID.mkScopedName) \mu -> do
        u <- mu
        (sc, n) <- UUID.getName u
        pure (dropZeroesEnd sc, dropZeroesEnd n)
  where
    genName =
        fmap (dropZeroesEnd . BS.pack) $
            Gen.list (Range.linear 1 10) $
                Gen.element $
                    BS.unpack Base64.alphabet
    dropZeroesEnd = fst . BSC.spanEnd (== '0')

prop_ron_json_example =
    let
        input =
            [s| *lww    #1TUAQ+gritzko @`   :bar = 1  ;
                        #(R            @`   :foo > (Q ;
            |]
        output =
            [ Closed
                ClosedOp
                    { reducerId = lwwType
                    , objectId = bar
                    , op =
                        Op{opId = bar, refId = barName, payload = [AInteger 1]}
                    }
            , Closed
                ClosedOp
                    { reducerId = lwwType
                    , objectId = foo
                    , op =
                        Op{opId = foo, refId = fooName, payload = [AUuid bar]}
                    }
            ]
        barName = $(UUID.liftName "bar")
        bar =
            encodeEvent $
                mkCalendarEvent
                    (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 26, 00))
                    replicaGritzko
        fooName = $(UUID.liftName "foo")
        foo =
            encodeEvent $
                mkCalendarEvent
                    (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 27, 00))
                    replicaGritzko
        gritzko = fromJust $ Base64.decode60 "gritzko"
        replicaGritzko = mkReplica TrieForked gritzko
     in
        property do
            parsed <- evalEitherS $ RT.parseWireFrame input
            output === parsed

lwwType = $(UUID.liftName "lww")

prop_lwwStruct = LwwStruct.prop_lwwStruct

prop_structSet = StructSet.prop_structSet

prop_orSet = ORSet.prop_orSet

prop_RGA_edit_idempotency = property do
    textX <- forAll Gen.shortText
    textY <- forAll Gen.shortText
    evalExceptT $
        runNetworkSimT $
            runReplicaSimT (mkReplica ApplicationSpecific 271) do
                rgaX <- newObjectFrameWith $ RGA.newFromText textX
                rgaY <- execObjectState rgaX $ RGA.editText textY
                textY' <- evalObjectState rgaY RGA.getText
                textY === textY'

prop_RGA_edit_idempotency_back = property do
    textX <- forAll Gen.shortText
    textY <- forAll Gen.shortText
    evalExceptT $
        runNetworkSimT $
            runReplicaSimT (mkReplica ApplicationSpecific 271) do
                rgaX <- newObjectFrameWith $ RGA.newFromText textX
                rgaY <- execObjectState rgaX $ RGA.editText textY
                rgaX' <- execObjectState rgaY $ RGA.editText textX
                textX' <- evalObjectState rgaX' RGA.getText
                textX === textX'

prop_RGA_delete_deleted =
    let
        rga0expect =
            prep
                [s| *rga    #7/0000000Dqr+000000003f                    !
                                                        @`}LxE          'h'
                                                        @)F             'e'
                                                        @)G             'l'
                                                        @)H             'l'
                                                        @)I             'o'
                    . |]
        rga1expect =
            prep
                [s| *rga    #7/0000000Dqr+000000003f                    !
                                                        @`}LxE          'h'
                                                        @)F             'e'
                                                        @)G             'l'
                                                        @)H             'l'
                                                        @)I     :`}Ykz  'o'
                    . |]
        rga2expect =
            prep
                [s| *rga    #7/0000000Dqr+000000003f                    !
                                                        @`}LxE          'h'
                                                        @)F             'e'
                                                        @)G             'l'
                                                        @)H     :`}bXU  'l'
                                                        @)I     :}Ykz   'o'
                                                        @}rSU   :0      'p'
                    . |]
     in
        property $
            ( evalExceptT
                . runNetworkSimT
                . runReplicaSimT (mkReplica ApplicationSpecific 234)
            )
                do
                    rga0 <- newObjectFrameWith $ RGA.newFromText "hello"
                    rga0expect === prepObj rga0

                    rga1 <- execObjectState rga0 $ RGA.editText "hell"
                    rga1expect === prepObj rga1

                    rga2 <- execObjectState rga1 $ RGA.editText "help"
                    rga2expect === prepObj rga2

prop_RGA_getAliveIndices = property do
    text <- forAll Gen.shortText
    replica <- forAll Gen.anyReplica
    evalExceptT $ runNetworkSimT do
        rga <-
            runReplicaSimT replica $ newObjectFrameWith $ RGA.newFromText text
        indices <- evalObjectState rga RGA.getAliveIndices
        Text.length text === length indices

-- TODO(2019-04-17, #60, cblp) RGA objects are not random enough. Try to
-- generate RGAs from a series of ops.
prop_RGA_insertAfter = property do
    (prefix, inset, suffix) <- forAll $ replicateM3 Gen.shortText
    (replica1, replica2) <- forAll $ replicateM2 Gen.replicaA
    evalExceptT $ runNetworkSimT do
        rgaState <-
            runReplicaSimT replica1 $
                newObjectFrameWith $
                    RGA.newFromText $
                        prefix <> suffix
        rgaIndices <- evalObjectState rgaState RGA.getAliveIndices
        annotateShow rgaIndices
        let pos = rgaIndices !! (Text.length prefix - 1)
        annotateShow pos
        text' <-
            runReplicaSimT replica2 $
                evalObjectState rgaState do
                    RGA.insertText inset pos
                    RGA.getText
        prefix <> inset <> suffix === text'

prop_RGA_remove = property do
    text <- forAll $ Gen.filter (not . Text.null) Gen.shortText
    i <- forAll $ Gen.int $ Range.linear 0 (Text.length text - 1)
    (replica1, replica2) <- forAll $ replicateM2 Gen.replicaA
    evalExceptT $ runNetworkSimT do
        rgaState <-
            runReplicaSimT replica1 $ newObjectFrameWith $ RGA.newFromText text
        indices <- evalObjectState rgaState RGA.getAliveIndices
        let u = fromJust $ indices !! i
        text' <-
            runReplicaSimT replica2 $ evalObjectState rgaState do
                RGA.remove u
                RGA.getText
        text_delete i text === text'
  where
    text_delete i t =
        let (before, after) = Text.splitAt i t in before <> Text.tail after

prop_base64_isLetter = property do
    c <- forAll $ Gen.word8 Range.constantBounded
    (c `BS.elem` Base64.alphabet) === Base64.isLetter c

data ShowAs a = ShowAs a String

instance Show (ShowAs a) where
    show (ShowAs _ str) = str

prop_ORSet =
    let
        state0expect =
            prep
                [s| *set #7/0000000Don+000000005j !
                    . |]
        state1expect =
            prep
                [s| *set #7/0000000Don+000000005j !
                    @`}HJ2 370
                    . |]
        state2expect =
            prep
                [s| *set #7/0000000Don+000000005j !
                    @`}U_Y :`}HJ2 370
                    . |]
     in
        property $
            evalExceptT $
                runNetworkSimT $
                    runReplicaSimT (mkReplica ApplicationSpecific 366) do
                        state0 <- newObjectFrame $ ORSet @Int64 []
                        state0expect === prepObj state0

                        state1 <- execObjectState state0 $ ORSet.addValue 370
                        state1expect === prepObj state1

                        state2 <- execObjectState state1 $ ORSet.removeValue 370
                        state2expect === prepObj state2

prop_ObjectORSet =
    let
        state0expect =
            prep
                [s| *set #7/0000000DlG+000000006G                   !
                    . |]
        state1expect =
            prep
                [s| *set #7/0000000DlG+000000006G                   !
                                                    @`}ih_          >}PsG
                    *rga #}PsG                      @0              !
                                                    @`}aHG          '4'
                                                    @)H             '0'
                                                    @)I             '3'
                    . |]
        state2expect =
            prep
                [s| *set #7/0000000DlG+000000006G                   !
                                                    @`}vyl  :`}ih_  >}PsG
                    *rga #}PsG                      @0      :0      !
                                                    @`}aHG          '4'
                                                    @)H             '0'
                                                    @)I             '3'
                    . |]
     in
        property $
            evalExceptT $
                runNetworkSimT $
                    runReplicaSimT (mkReplica ApplicationSpecific 400) do
                        state0 <- newObjectFrame $ ORSet @RgaString []
                        state0expect === prepObj state0

                        (rga, state1) <- runObjectState state0 do
                            rga <- RGA.newFromText "403"
                            ORSet.addRef rga
                            pure rga
                        state1expect === prepObj state1

                        state2 <- execObjectState state1 $ ORSet.removeRef rga
                        state2expect === prepObj state2

prop_ObjectORSet_recursive =
    let
        state0 = TestRecursiveORSet{testRecSet = Just $ ORSet []}
        state1expect =
            prep
                [s| *lww #7/0000000Dl7+000000006P !
                        @` :testRecSet >}NbH
                    *set #}NbH @0 :0 !
                    . |]
        state2expect =
            prep
                [s| *lww #7/0000000Dl7+000000006P !
                        @` :testRecSet >}NbH
                    *set #}NbH @0 :0 !
                        @`}_gy >}Dl7
                    . |]
     in
        property $
            evalExceptT $
                runNetworkSimT $
                    runReplicaSimT (mkReplica ApplicationSpecific 409) do
                        state1 <- newObjectFrame state0
                        state1expect === prepObj state1

                        state2 <-
                            execObjectState state1 do
                                outerSet <- ask
                                testRecSet_zoom $ ORSet.addRef outerSet
                        state2expect === prepObj state2

prepObj :: ObjectFrame a -> [[ByteStringL]]
prepObj = prep . snd . RT.serializeObject

prep :: ByteStringL -> [[ByteStringL]]
prep = filter (not . null) . map BSLC.words . BSLC.lines

-- example taken from https://replicated.cc/
replicatedCcExampleLaptopText =
    TextL.encodeUtf8
        [s|
        @1fLDV+biQFvtGV :lww,
                            'id'        '20MF000CUS',
                            'type'      'laptop',
                            'cpu'       'i7-8850H',
                            'display'   '15.6” UHD IPS multi-touch, 400nits',
                            'RAM'       '16 GB DDR4 2666MHz',
                            'storage'   '512 GB SSD, PCIe-NVME M.2',
                            'graphics'  'NVIDIA GeForce GTX 1050Ti 4GB',
        @1fLDk4+biQFvtGV    'wlan'      'Intel 9560 802.11AC vPro',
                            'camera'    'IR & 720p HD Camera with microphone',
        @sha3               'SfiKqD1atGU5xxv1NLp8uZbAcHQDcX~a1HVk5rQFy_nq';
        |]

replicatedCcExampleLaptopFrame =
    [ Op (i 0) lwwType []
    , Op (i 1) (i 0) ["id", "20MF000CUS"]
    , Op (i 2) (i 1) ["type", "laptop"]
    , Op (i 3) (i 2) ["cpu", "i7-8850H"]
    , Op (i 4) (i 3) ["display", "15.6” UHD IPS multi-touch, 400nits"]
    , Op (i 5) (i 4) ["RAM", "16 GB DDR4 2666MHz"]
    , Op (i 6) (i 5) ["storage", "512 GB SSD, PCIe-NVME M.2"]
    , Op (i 7) (i 6) ["graphics", "NVIDIA GeForce GTX 1050Ti 4GB"]
    , Op (j 0) (i 7) ["wlan", "Intel 9560 802.11AC vPro"]
    , Op (j 1) (j 0) ["camera", "IR & 720p HD Camera with microphone"]
    , Op sha3 (j 1) ["SfiKqD1atGU5xxv1NLp8uZbAcHQDcX~a1HVk5rQFy_nq"]
    ]
  where
    object = UUID 0x_006a_54d7_c000_0000 0x_29ad_68fe_b841_f000
    i n = object & UUID.value +~ n -- identifier from session 1fLDV
    j n =
        -- identifier from session 1fLDk4
        object & UUID.value +~ 0x4_0400_0000 + n
    sha3 = $(UUID.liftName "sha3")

prop_replicatedCcExampleLaptopParse =
    property do
        parsed <- evalEitherS $ RT.parseOpenFrame replicatedCcExampleLaptopText
        replicatedCcExampleLaptopFrame === parsed

prop_replicatedCcExampleLaptopSerialize =
    property $
        prep replicatedCcExampleLaptopText
            === prep (RT.serializeOpenFrame replicatedCcExampleLaptopFrame)
