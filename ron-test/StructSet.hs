{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StructSet (prop_structSet) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Default (def)
import qualified Data.Map.Strict as Map
import           Hedgehog (MonadTest, Property, annotate, evalEither,
                           evalExceptT, failure, property, (===))

import           RON.Data (evalObjectState, execObjectState, getObject,
                           newObjectFrame)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import           RON.Text (parseObject, serializeObject)
import           RON.Types (Object, Op (Op), StateChunk (StateChunk),
                            StateFrame, opId, refId, stateBody, stateType)
import           RON.Util (ByteStringL)
import           RON.UUID (zero)

import           Orphans ()
import           String (s)
import           StructSet.Types

example0 :: StructSet13
example0 = StructSet13
    { int1 = Just 275
    , str2 = Just $ RGA "275"
    , str3 = Just "190"
    , set4 = Just $ ORSet []
    , opt5 = Just Nothing
    , nst6 = Nothing
    }

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000

state1expect :: ByteStringL
state1expect = [s|
    *set    #B/0000000DrW+r3pl1c4           !
                                    @`}KUW  >int1   275
                                    @}OUW   >opt5
                                    @}WUW   >set4   >B/0000000lUW+r3pl1c4
                                    @{10UW  >str2   >B/0000001GUW+r3pl1c4
                                    @}LcW   >str3   '190'

            #}lUW                   @0      !

    *rga    #{1GUW                          !
                                    @`]g6   '2'
                                    @)7     '7'
                                    @)8     '5'
    .
    |]

state4expect :: ByteStringL
state4expect = [s|
    *set    #B/0000000DrW+r3pl1c4                   !
                                    @`}KUW  :`{1k2W >int1
                                    @}OUW   :0      >opt5
                                    @}WUW           >set4 >B/0000000lUW+r3pl1c4
                                    @{10UW          >str2 >B/0000001GUW+r3pl1c4
                                    @}LcW   :`{2WUW >str3
                                    @}ZdW   :0      >int1 166
                                    @{2OUW          >str3 '206'
                                    @{3~2W          >nst6 >B/00000042MW+r3pl1c4
            #}lUW                   @0              !
                                    @`{3odW         >{2lUW
    *rga    #{1GUW                  @0              !
                                    @`]g6   :`}nMW  '2'
                                    @)7     :{21UW  '7'
                                    @{2AUW  :0      '1'
                                    @}KUW           '4'
                                    @`]g8           '5'
    *set    #{2lUW                  @0              !
                                    @`{30UW         >int1 135
                                    @}GUW           >str2 >B/0000003WUW+r3pl1c4
                                    @}acW           >str3 '137'
    *rga    #{3WUW                  @0              !
                                    @`]g6           '1'
                                    @)7             '3'
                                    @)8             '6'
    *set    #{42MW                  @0              !
                                    @`}HUW          >int1 138
    .
    |]

example4expect :: StructSet13
example4expect = StructSet13
    { int1 = Just 166
    , str2 = Just $ RGA "145"
    , str3 = Just "206"
    , set4 = Just $
        ORSet [def{int1 = Just 135, str2 = Just $ RGA "136", str3 = Just "137"}]
    , opt5 = Just Nothing
    , nst6 = Just def{int1 = Just 138}
    }

prop_structSet :: Property
prop_structSet = property $ do
    -- create an object
    state1 <- runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example0
    let (oid, state1ser) = serializeObject state1
    prep state1expect === prep state1ser

    -- parse newly created object
    state2 <- evalEither $ parseObject oid state1ser
    state1 === state2

    -- decode newly created object
    example3 <- evalEither $ evalObjectState state2 getObject
    example0 === example3

    -- apply operations to the object (frame)
    state4 <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        execObjectState state2 $ do
            checkCausality
            int1_assign 166 -- plain field
            checkCausality
            str2_zoom $ RGA.edit "145"
            checkCausality
            do  value <- str3_read
                value === Just "190"
            str3_assign "206"
            checkCausality
            set4_zoom $ do
                ORSet.addValue
                    def { int1 = Just 135
                        , str2 = Just $ RGA "136"
                        , str3 = Just "137"
                        }
                checkCausality
            checkCausality
            do  value <- opt5_read
                value === Just Nothing
            do  value <- nst6_read
                value === Nothing
            nst6_assign def{int1 = Just 138}
            checkCausality

    -- decode object after modification
    example4 <- evalEither $ evalObjectState state4 getObject
    example4expect === example4

    -- serialize object after modification
    prep state4expect === prep (snd $ serializeObject state4)

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines

checkCausality
    ::  ( HasCallStack
        , MonadTest m
        , MonadReader (Object a) m
        , MonadState StateFrame m
        , Typeable a
        )
    =>  m ()
checkCausality = do
    root <- ask
    get >>= checkStateFrame root
  where
    checkStateFrame root = void . Map.traverseWithKey (checkObject root)
    checkObject root self StateChunk{stateType, stateBody} =
        for_ stateBody $ \Op{opId, refId} -> do
            unless (opId > self) $ do
                annotate $ unlines
                    [ "root = " <> show root
                    , "self = " <> show self <> " :: " <> show stateType
                    , "opId = " <> show opId
                    ]
                failure
            unless (refId == zero || refId > self) $ do
                annotate $ unlines
                    [ "root = " <> show root
                    , "self = " <> show self <> " :: " <> show stateType
                    , "refId = " <> show refId
                    ]
                failure
