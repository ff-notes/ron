{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CT (prop_causalTree0, prop_causalTree1) where

import RON.Prelude

import Data.ByteString.Lazy.Char8 qualified as BSLC
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
    newObjectFrame,
    readObject,
 )
import RON.Data.CT (CT (CT), CTString)
import RON.Data.CT qualified as CT
import RON.Event (
    OriginVariety (ApplicationSpecific),
    Replica,
    mkReplica,
 )
import RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import RON.Text (parseObject, serializeObject)
import RON.Types (
    ObjectRef,
    Op (Op),
    StateFrame,
    WireStateChunk (WireStateChunk),
 )
import RON.Types qualified
import RON.UUID (zero)

import Orphans ()
import String (s)

example0 :: CTString
example0 = CT "abc[DEL]ghi[INS:]jkl"

-- | "r3pl1c4"
replica :: Replica
replica = mkReplica ApplicationSpecific 0xd83d30067100000

state1expect :: ByteStringL
state1expect =
    [s| *ct #7/0000000DrW+r3pl1c4                   !
                                    @`}KUW          'a'
                                    @)X     :`)W    'b'
                                    @)Y     :)X     'c'
                                    @)Z     :)Y     '['
                                    @)_     :)Z     'D'
                                    @)a     :)_     'E'
                                    @)b     :)a     'L'
                                    @)c     :)b     ']'
                                    @)d     :)c     'g'
                                    @)e     :)d     'h'
                                    @)f     :)e     'i'
                                    @)g     :)f     '['
                                    @)h     :)g     'I'
                                    @)i     :)h     'N'
                                    @)j     :)i     'S'
                                    @)k     :)j     ':'
                                    @)l     :)k     ']'
                                    @)m     :)l     'j'
                                    @)n     :)m     'k'
                                    @)o     :)n     'l'
        . |]

state4expect :: ByteStringL
state4expect =
    [s| *ct #7/0000000DrW+r3pl1c4                   !
                                    @`}KUW          'a'
                                    @)X     :`)W    'b'
                                    @)Y     :)X     'c'
                                    @)Z     :)Y     '['
                                    @)_     :)Z     'D'
                                    @)a     :)_     'E'
                                    @)b     :)a     'L'
                                    @)c     :)b     ']'
                                    @)d     :)c     'g'
                                    @)e     :)d     'h'
                                    @)f     :)e     'i'
                                    @)g     :)f     '['
                                    @)h     :)g     'I'
                                    @)i     :)h     'N'
                                    @)j     :)i     'S'
                                    @)k     :)j     ':'
                                    @)l     :)k     ']'
                                    @)m     :)l     'j'
                                    @)n     :)m     'k'
                                    @)o     :)n     'l'
                                    @}PeO   :)_
                                    @)P     :)a
                                    @)Q     :)b
                                    @}b1K   :)k     'm'
                                    @)L     :`)K    'n'
                                    @)M     :)L     'o'
        . |]

example4expect :: CTString
example4expect = CT "abc[]ghi[INS:mno]jkl"

prop_causalTree0 :: Property
prop_causalTree0 = property do
    -- create an object
    state1 <- runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example0
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
        ( evalExceptT
                . runNetworkSimT
                . runReplicaSimT replica
                . execObjectState state2
            )
            do
                checkCausality
                CT.edit "abc[]ghi[INS:mno]jkl"
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
    , MonadTest m
    , MonadReader (ObjectRef a) m
    , MonadState StateFrame m
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

example10 :: CTString
example10 = CT "Erik"

ex11expect :: ByteStringL
ex11expect =
    [s| *ct #7/0000000DrW+r3pl1c4                   !
                                    @`}KUW          'E'
                                    @)X     :`)W    'r'
                                    @)Y     :)X     'i'
                                    @)Z     :)Y     'k'
        . |]

ex14expect :: ByteStringL
ex14expect =
    [s| *ct #7/0000000DrW+r3pl1c4                   !
                                    @`}KUW          'E'
                                    @)X     :`)W    'r'
                                    @)Y     :)X     'i'
                                    @)Z     :)Y     'k'
                                    @}VEt   :)W
                                    @)u     :)X
                                    @)v     :)Y
                                    @)w     :)Z
                                    @]xO    :}VEw   'A'
                                    @)P     :]xO    'd'
                                    @)Q     :)P     'a'
        . |]

example14expect :: CTString
example14expect = CT "Ada"

prop_causalTree1 :: Property
prop_causalTree1 = property do
    -- create an object
    ex11state <-
        runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example10
    let (oid, ex11ser) = serializeObject ex11state
    prep ex11expect === prep ex11ser

    -- parse newly created object
    ex12state <- evalEither $ parseObject oid ex11ser
    ex11state === ex12state

    -- decode newly created object
    example13 <- evalEither $ evalObjectState ex12state readObject
    example10 === example13

    -- apply operations to the object (frame)
    ex14state <-
        ( evalExceptT
                . runNetworkSimT
                . runReplicaSimT replica
                . execObjectState ex12state
            )
            do
                checkCausality
                CT.edit "Ada"
                checkCausality

    -- decode object after modification
    example14 <- evalEither $ evalObjectState ex14state readObject
    example14expect === example14

    -- serialize object after modification
    prep ex14expect === prep (snd $ serializeObject ex14state)
    parseObject oid ex14expect === Right ex14state
