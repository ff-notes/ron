{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import           RON.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Hedgehog (Gen, MonadTest, Property, PropertyT, annotate,
                           annotateShow, evalExceptT, forAll, property,
                           tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Directory (getCurrentDirectory)
import           System.Environment (getEnv, lookupEnv, setEnv)
import           System.Info (os)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)
import qualified Text.Show

import qualified RON.Base64 as Base64
import qualified RON.Binary.Parse as RB
import qualified RON.Binary.Serialize as RB
import           RON.Data (evalObjectState, execObjectState, newObjectFrame,
                           newObjectFrameWith, runObjectState)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RgaString)
import qualified RON.Data.RGA as RGA
import           RON.Event (CalendarEvent (CalendarEvent), Naming (TrieForked),
                            ReplicaId (ReplicaId), applicationSpecific,
                            decodeEvent, encodeEvent, fromCalendarEvent,
                            mkCalendarDateTime)
import           RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import qualified RON.Text as RT
import qualified RON.Text.Parse as RT
import qualified RON.Text.Serialize as RT
import qualified RON.Text.Serialize.UUID as RT
import           RON.Types (Atom (AInteger, AUuid),
                            ClosedOp (ClosedOp, objectId, op, reducerId),
                            ObjectFrame, Op (Op, opId, payload, refId),
                            UUID (UUID), WireChunk (Closed))
import           RON.Util (ByteStringL)
import qualified RON.UUID as UUID

import qualified Gen
import           HexDump (hexdump)
import qualified LwwStruct
import           Orphans ()
import qualified ORSet
import           String (s)
import qualified StructSet
import           Types (TestRecursiveORSet (TestRecursiveORSet), testRecSet,
                        testRecSet_zoom)

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
        _        -> error $ os ++ " is not supported"

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
    -- TODO(2018-11-08, cblp, #4) increase limits
    binaryRoundtrip (Gen.atom 1000) RB.serializeAtom RB.parseAtom

-- TODO(2018-10-02, cblp, #23) fix and enable
-- prop_binary_roundtrip_frame =
--     -- TODO(2018-11-08, cblp, #4) increase limits
--     binaryRoundtrip (Gen.wireFrame 10) RB.serialize RB.parse

textRoundtrip
    :: (Show a, Show b, Applicative f, Eq (f a), Show (f a), Monad m)
    => Gen a -> (a -> b) -> (b -> f a) -> PropertyT m ()
textRoundtrip gen serialize parse = do
    x <- forAll gen
    tripping x serialize parse

prop_text_roundtrip_uuid =
    property $ textRoundtrip Gen.uuid RT.serializeUuid RT.parseUuid

prop_text_roundtrip_uuid_zip = property $ do
    prevKey <- forAll Gen.uuid
    prev    <- forAll Gen.uuid
    textRoundtrip
        Gen.uuid
        (RT.serializeUuidKey prevKey prev)
        (RT.parseUuidKey prevKey prev)
    textRoundtrip Gen.uuid (RT.serializeUuidAtom prev) (RT.parseUuidAtom prev)

prop_text_roundtrip_string = property $
    textRoundtrip
        (Gen.text (Range.exponential 0 10000) Gen.unicode)
        RT.serializeString
        RT.parseString

prop_text_roundtrip_atom =
    property $ textRoundtrip (Gen.atom 10000) RT.serializeAtom RT.parseAtom

prop_text_roundtrip_op =
    -- TODO(2018-11-08, cblp, #4) increase limits
    property $ textRoundtrip (Gen.closedOp 100) RT.serializeRawOp RT.parseOp

prop_text_roundtrip_wireFrame = property $
    -- TODO(2018-11-08, cblp, #4) increase limits
    textRoundtrip (Gen.wireFrame 10) RT.serializeWireFrame RT.parseWireFrame

prop_text_roundtrip_stateFrame = property $
    -- TODO(2018-11-08, cblp, #4) increase limits
    textRoundtrip (Gen.stateFrame 10) RT.serializeStateFrame RT.parseStateFrame

prop_text_roundtrip_frames = property $
    -- TODO(2018-11-08, cblp, #4) increase limits
    textRoundtrip (Gen.wireFrames 10) RT.serializeWireFrames RT.parseWireFrames

prop_filename_roundtrip = property $ do
    ShowAs caseTransform _ <- forAll $ Gen.element
        [ id          `ShowAs` "id"
        , map toUpper `ShowAs` "map toUpper"
        , map toLower `ShowAs` "map toLower"
        ]
    textRoundtrip
        Gen.uuid
        (BSLC.pack . caseTransform . UUID.encodeBase32)
        (maybe (Left ("Filename decoding error" :: String)) Right .
            UUID.decodeBase32 . BSLC.unpack)

prop_word64base32_roundtrip = property $
    textRoundtrip
        Gen.word64'
        (fromStrict . Base64.encode64base32short)
        (maybe (Left ("Decoding error" :: String)) Right .
            Base64.decode64base32 . BSLC.toStrict)

prop_base64_roundtrip = property $ do
    bytes <- forAll $ fromStrict <$> Gen.bytes (Range.exponential 0 1000)
    tripping bytes Base64.encode Base64.decode

prop_word60base64_roundtrip = property $ do
    w <- forAll Gen.word60
    tripping w Base64.encode60 Base64.decode60

prop_long_uuid = property $
    Right (UUID 0xa001083105187209 0x89669e8a6aaecb6e) ===
    RT.parseUuid "A01234567898abcdefghij"

prop_uuid_abbreviations = property $ do
    -- serialize
    "A/LED" === RT.serializeUuid aLed
    -- parse
    for_ encodings $ \e -> do
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

prop_event_roundtrip = property $ do
    event <- forAll Gen.event
    tripping event encodeEvent (Just . decodeEvent)

prop_name_roundtip = property $ do
    name <- forAll genName
    tripping name UUID.mkName $ \mu -> do
        u <- mu
        (n, "") <- UUID.getName u
        pure $ dropZeroesEnd n
    scope <- forAll genName
    tripping (scope, name) (uncurry UUID.mkScopedName) $ \mu -> do
        u <- mu
        (sc, n) <- UUID.getName u
        pure (dropZeroesEnd sc, dropZeroesEnd n)
  where
    genName
        = fmap (dropZeroesEnd . BS.pack) $ Gen.list (Range.linear 1 10)
        $ Gen.element $ BS.unpack Base64.alphabet
    dropZeroesEnd = fst . BSC.spanEnd (== '0')

prop_ron_json_example = let
    input = [s|
        *lww #1TUAQ+gritzko @`   :bar = 1  ;
             #(R            @`   :foo > (Q ;
        |]
    output =
        [ Closed ClosedOp
            { reducerId = lwwType
            , objectId = bar
            , op = Op{opId = bar, refId = barName, payload = [AInteger 1]}
            }
        , Closed ClosedOp
            { reducerId = lwwType
            , objectId = foo
            , op = Op{opId = foo, refId = fooName, payload = [AUuid bar]}
            }
        ]
    barName = $(UUID.liftName "bar")
    bar     = encodeEvent $ fromCalendarEvent $ CalendarEvent
                (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 26, 00))
                replicaGritzko
    fooName = $(UUID.liftName "foo")
    foo     = encodeEvent $ fromCalendarEvent $ CalendarEvent
                (fromJust $ mkCalendarDateTime (2017, 10, 31) (10, 27, 00))
                replicaGritzko
    gritzko = fromJust $ Base64.decode60 "gritzko"
    replicaGritzko = ReplicaId TrieForked gritzko
    in
    property $ do
        parsed <- evalEitherS $ RT.parseWireFrame input
        output === parsed

lwwType = $(UUID.liftName "lww")

prop_lwwStruct = LwwStruct.prop_lwwStruct

prop_structSet = StructSet.prop_structSet

prop_orSet = ORSet.prop_orSet

prop_RGA_edit_idempotency = property $ do
    textX <- forAll Gen.shortText
    textY <- forAll Gen.shortText
    evalExceptT $
        runNetworkSimT $ runReplicaSimT (applicationSpecific 271) $ do
            rgaX   <- newObjectFrameWith   $ RGA.newFromText textX
            rgaY   <- execObjectState rgaX $ RGA.editText    textY
            textY' <- evalObjectState rgaY   RGA.getText
            textY === textY'

prop_RGA_edit_idempotency_back = property $ do
    textX <- forAll Gen.shortText
    textY <- forAll Gen.shortText
    evalExceptT $
        runNetworkSimT $ runReplicaSimT (applicationSpecific 271) $ do
            rgaX   <- newObjectFrameWith   $ RGA.newFromText textX
            rgaY   <- execObjectState rgaX $ RGA.editText    textY
            rgaX'  <- execObjectState rgaY $ RGA.editText    textX
            textX' <- evalObjectState rgaX'  RGA.getText
            textX === textX'

prop_RGA_delete_deleted = let
    rga0expect = prep [s|
        *rga    #B/0000000Dqr+000000003f                    !
                                            @`}LaI          'h'
                                            @)J             'e'
                                            @)K             'l'
                                            @)L             'l'
                                            @)M             'o'
        . |]
    rga1expect = prep [s|
        *rga    #B/0000000Dqr+000000003f                    !
                                            @`}LaI          'h'
                                            @)J             'e'
                                            @)K             'l'
                                            @)L             'l'
                                            @)M     :`}WFE  'o'
        . |]
    rga2expect = prep [s|
        *rga    #B/0000000Dqr+000000003f                    !
                                            @`}LaI          'h'
                                            @)J             'e'
                                            @)K             'l'
                                            @)L     :`}cyz  'l'
                                            @)M     :}WFE   'o'
                                            @}kmU   :0      'p'
        . |]
    in
    property $ evalExceptT $
    runNetworkSimT $ runReplicaSimT (applicationSpecific 234) $ do
        rga0 <- newObjectFrameWith $ RGA.newFromText "hello"
        rga0expect === prepObj rga0

        rga1 <- execObjectState rga0 $ RGA.editText "hell"
        rga1expect === prepObj rga1

        rga2 <- execObjectState rga1 $ RGA.editText "help"
        rga2expect === prepObj rga2

prop_RGA_getAliveIndices = property $ do
    text    <- forAll Gen.shortText
    replica <- forAll Gen.replicaId
    evalExceptT $ runNetworkSimT $ do
        rga <-
            runReplicaSimT replica $ newObjectFrameWith $ RGA.newFromText text
        indices <- evalObjectState rga RGA.getAliveIndices
        Text.length text === length indices

-- TODO(2019-04-17, #60, cblp) RGA objects are not random enough. Try to
-- generate RGAs from a series of ops.
prop_RGA_insertAfter = property $ do
    (prefix, inset, suffix) <- forAll $ replicateM3 Gen.shortText
    (replica1, replica2)    <- forAll $ replicateM2 Gen.replicaId
    evalExceptT $ runNetworkSimT $ do
        rgaState <-
            runReplicaSimT replica1 $
            newObjectFrameWith $ RGA.newFromText $ prefix <> suffix
        rgaIndices <- evalObjectState rgaState RGA.getAliveIndices
        annotateShow rgaIndices
        let pos = rgaIndices !! (Text.length prefix - 1)
        annotateShow pos
        text' <-
            runReplicaSimT replica2 $
            evalObjectState rgaState $ do
                RGA.insertText inset pos
                RGA.getText
        prefix <> inset <> suffix === text'

prop_RGA_remove = property $ do
    text <- forAll $ Gen.filter (not . Text.null) Gen.shortText
    i <- forAll $ Gen.int $ Range.linear 0 (Text.length text - 1)
    (replica1, replica2) <- forAll $ replicateM2 Gen.replicaId
    evalExceptT $ runNetworkSimT $ do
        rgaState <-
            runReplicaSimT replica1 $ newObjectFrameWith $ RGA.newFromText text
        indices <- evalObjectState rgaState RGA.getAliveIndices
        let u = fromJust $ indices !! i
        text' <-
            runReplicaSimT replica2 $ evalObjectState rgaState $ do
                RGA.remove u
                RGA.getText
        text_delete i text === text'
  where
    text_delete i t =
        let (before, after) = Text.splitAt i t in before <> Text.tail after

prop_base64_isLetter = property $ do
    c <- forAll $ Gen.word8 Range.constantBounded
    (c `BS.elem` Base64.alphabet) === Base64.isLetter c

data ShowAs a = ShowAs a String

instance Show (ShowAs a) where
    show (ShowAs _ str) = str

prop_ORSet = let
    state0expect = prep [s|
        *set #B/0000000Don+000000005j !
        . |]
    state1expect = prep [s|
        *set #B/0000000Don+000000005j !
        @`}HJ2 370
        . |]
    state2expect = prep [s|
        *set #B/0000000Don+000000005j !
        @`}U_Y :`}HJ2 370
        . |]
    in
    property $ evalExceptT $
    runNetworkSimT $ runReplicaSimT (applicationSpecific 366) $ do
        state0 <- newObjectFrame $ ORSet @Int64 []
        state0expect === prepObj state0

        state1 <- execObjectState state0 $ ORSet.addValue 370
        state1expect === prepObj state1

        state2 <- execObjectState state1 $ ORSet.removeValue 370
        state2expect === prepObj state2

prop_ObjectORSet = let
    state0expect = prep [s|
        *set #B/0000000DlG+000000006G                   !
        . |]
    state1expect = prep [s|
        *set #B/0000000DlG+000000006G                   !
                                        @`}ghG          >}PsG
        *rga #}PsG                      @0              !
                                        @`}aXM          '4'
                                        @)N             '0'
                                        @)O             '3'
        . |]
    state2expect = prep [s|
        *set #B/0000000DlG+000000006G                   !
                                        @`}psG  :`}ghG  >}PsG
        *rga #}PsG                      @0      :0      !
                                        @`}aXM          '4'
                                        @)N             '0'
                                        @)O             '3'
        . |]
    in
    property $ evalExceptT $
    runNetworkSimT $ runReplicaSimT (applicationSpecific 400) $ do
        state0 <- newObjectFrame $ ORSet @RgaString []
        state0expect === prepObj state0

        (rga, state1) <- runObjectState state0 $ do
            rga <- RGA.newFromText "403"
            ORSet.addRef rga
            pure rga
        state1expect === prepObj state1

        state2 <- execObjectState state1 $ ORSet.removeRef rga
        state2expect === prepObj state2

prop_ObjectORSet_recursive = let
    state0 = TestRecursiveORSet{testRecSet = ORSet []}
    state1expect = prep [s|
        *lww #B/0000000Dl7+000000006P !
            @` :testRecSet >}NbH
        *set #}NbH @0 :0 !
        . |]
    state2expect = prep [s|
        *lww #B/0000000Dl7+000000006P !
            @` :testRecSet >}NbH
        *set #}NbH @0 :0 !
            @`}_gy >}Dl7
        . |]
    in
    property $ evalExceptT $
    runNetworkSimT $ runReplicaSimT (applicationSpecific 409) $ do
        state1 <- newObjectFrame state0
        state1expect === prepObj state1

        state2 <-
            execObjectState state1 $ do
                outerSet <- ask
                testRecSet_zoom $ ORSet.addRef outerSet
        state2expect === prepObj state2

prepObj :: ObjectFrame a -> [[ByteStringL]]
prepObj = prep . snd . RT.serializeObject

prep :: ByteStringL -> [[ByteStringL]]
prep = filter (not . null) . map BSLC.words . BSLC.lines
