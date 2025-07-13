{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StructSet (prop_structSet) where

import RON.Prelude

import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Default (def)
import Data.Map.Strict qualified as Map
import GHC.Stack (withFrozenCallStack)
import Hedgehog (
    MonadTest,
    Property,
    annotate,
    evalEither,
    evalExceptT,
    failure,
    property,
    (===),
 )

import RON.Data (
    evalObjectState,
    execObjectState,
    newObject,
    newObjectFrame,
    readObject,
 )
import RON.Data.CT (CT (CT))
import RON.Data.CT qualified as CT
import RON.Data.ORSet (ORSet (ORSet))
import RON.Data.ORSet qualified as ORSet
import RON.Data.RGA (RGA (RGA))
import RON.Data.RGA qualified as RGA
import RON.Event (
    OriginVariety (ApplicationSpecific),
    Replica,
    mkReplica,
 )
import RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import RON.Text (parseObject, serializeObject)
import RON.Types (
    ObjectRef,
    Op (Op, opId, refId),
    StateFrame,
    WireStateChunk (WireStateChunk, stateBody, stateType),
 )
import RON.UUID (zero)

import Orphans ()
import String (s)
import StructSet.Types

example0 :: StructSet13
example0 =
    StructSet13
        { int1 = Just 275
        , str2 = Just $ RGA "275"
        , str3 = Just "190"
        , set4 = Nothing
        , set5 = []
        , nst6 = Nothing
        , ref7 = []
        , str8 = Just $ CT "Anne"
        }

-- | "r3pl1c4"
replica :: Replica
replica = mkReplica ApplicationSpecific 0xd83d30067100000

state1expect :: ByteStringL
state1expect =
    [s| *set    #7/0000000DrW+r3pl1c4           !
                                @`}KUW          >int1   275
                                @}OUW           >str2   >7/0000000WUW+r3pl1c4
                                @}~mp           >str3   '190'
                                @{140W          >str8   >7/0000001H2W+r3pl1c4
        *rga    #}WUW           @0              !
                                @`}lUW          %f '275'
        *ct     #{1H2W          @0              !
                                @`}MMW          %c 'Anne'
        .
    |]

state4expect :: ByteStringL
state4expect =
    [s| *set    #7/0000000DrW+r3pl1c4   !
                        @`}OUW          >str2 >7/0000000WUW+r3pl1c4
                        @{140W          >str8 >7/0000001H2W+r3pl1c4
                        @}MMW           >int1 166
                        @}PUW   :`{0KUW >int1 275
                        @}VN4   :0      >str3 '206'
                        @}ZM    :`{0~mp >str3 '190'
                        @}lIW   :0      >set4 >7/0000001rrW+r3pl1c4
                        @{3H2W          >nst6 >7/0000003MMW+r3pl1c4
                        @{4WUW          >set5 172
                        @}lUW   :`}GUW  >set5 170
                        @{5WUW  :{5GUW  >ref7 >7/00000050UW+r3pl1c4
        *rga    #}WUW   @0      :0      !
                        @`}lUW  :`{1QUW %i '27'
                        @{1Rw_  :0      %f '14'
                        @{0lUY          '5'
        *ct     #{1H2W  @0              !
                        @`}MMW          %c 'Anne'
                        @{5lUW  :)W     %i 3
                        @}~mp   :{5lUY  'J'
                        @{640W  :{1MMZ  %c 'ssie'
        *set    #}rrW   @0      :0      !
                        @`{3lUW         >{3QUW
                        @{40UW  :`{340W >}zUW
                #}zUW   @0      :0      !
                        @`{2CUW         >int1 135
                        @}OUW           >str2 >7/0000002WUW+r3pl1c4
                        @}~mp           >str3 '137'
        *rga    #{2WUW  @0              !
                        @`}lUW          %f '136'
        *set    #{3MMW  @0              !
                        @`}PUW          >int1 138
                #}QUW   @0              !
                        @`}_UW          >int1 164
                        @}dUW           >str3 '166'
                #{50UW  @0              !
        .
    |]

example4expect :: StructSet13
example4expect =
    StructSet13
        { int1 = Just 166
        , str2 = Just $ RGA "145"
        , str3 = Just "206"
        , set4 = Just $ ORSet [def{int1 = Just 164, str3 = Just "166"}]
        , set5 = [172]
        , nst6 = Just def{int1 = Just 138}
        , ref7 = []
        , str8 = Just $ CT "Jessie"
        }

prop_structSet :: Property
prop_structSet =
    property do
        -- create an object
        state1 <-
            runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example0
        let (oid, state1ser) = serializeObject state1
        prep state1expect === prep state1ser

        -- parse newly created object
        state2 <- evalEither $ parseObject oid state1ser
        state1 === state2

        -- decode newly created object
        example3 <- evalEither $ evalObjectState state2 readObject
        example0 === example3

        -- apply operations to the object (frame)
        state4 <-
            evalExceptT
                . runNetworkSimT
                . runReplicaSimT replica
                $ execObjectState state2 do
                    checkCausality
                    int1_set 166 -- plain field
                    checkCausality
                    str2_zoom $ RGA.edit "145"
                    checkCausality
                    do
                        value <- str3_read
                        value === Just "190"
                    str3_set "206"
                    checkCausality
                    Nothing <- set4_get
                    set4_set $ ORSet []
                    Just set4ref1 <- set4_get
                    set4_zoom do
                        ORSet.addValue
                            def
                                { int1 = Just 135
                                , str2 = Just $ RGA "136"
                                , str3 = Just "137"
                                }
                        checkCausality
                    Just set4ref2 <- set4_get
                    set4ref1 === set4ref2
                    checkCausality
                    do
                        value <- nst6_read
                        value === Nothing
                    nst6_set def{int1 = Just 138}
                    checkCausality
                    set4_zoom do
                        ORSet.addValue
                            def
                                { int1 = Just 164
                                , str2 = Nothing
                                , str3 = Just "166"
                                }
                        ORSet.removeObjectIf do
                            i1 <- int1_read
                            pure $ i1 == Just 135
                    checkCausality
                    set5_add 170
                    set5_add 172
                    checkCausality
                    set5_remove 170
                    set5_remove 175 -- nothing changes
                    checkCausality
                    ss13 <- newObject def
                    ref7_add ss13
                    checkCausality
                    ref7_removeIf \r -> pure $ r == ss13
                    checkCausality
                    str8_zoom $ CT.edit "Jessie"
                    checkCausality

        -- decode object after modification
        example4 <- evalEither $ evalObjectState state4 readObject
        example4expect === example4

        -- serialize object after modification
        prep state4expect === prep (snd $ serializeObject state4)

prep :: ByteStringL -> [ByteStringL]
prep = filter (not . BSLC.null) . map (BSLC.unwords . BSLC.words) . BSLC.lines

checkCausality ::
    ( HasCallStack
    , MonadReader (ObjectRef a) m
    , MonadState StateFrame m
    , MonadTest m
    , Typeable a
    ) =>
    m ()
checkCausality = do
    root <- ask
    frame <- get
    checkStateFrame root frame
  where
    checkStateFrame root = void . Map.traverseWithKey (checkObject root)

    checkObject root self WireStateChunk{stateType, stateBody} =
        withFrozenCallStack $
            for_ stateBody \Op{opId, refId} -> do
                unless (opId > self) do
                    annotate $
                        unlines
                            [ "Expected opId > self"
                            , "root = " <> show root
                            , "self = " <> show self <> " :: " <> show stateType
                            , "opId = " <> show opId
                            ]
                    failure
                unless (refId == zero || refId >= self) do
                    annotate $
                        unlines
                            [ "Expected refId == zero || refId >= self"
                            , "root = " <> show root
                            , "self = " <> show self <> " :: " <> show stateType
                            , "refId = " <> show refId
                            ]
                    failure
