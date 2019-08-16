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

import           RON.Data (evalObjectState, execObjectState, newObjectFrame,
                           readObject)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import           RON.Text (parseObject, serializeObject)
import           RON.Types (Object, Op (Op, opId, refId), StateFrame, WireStateChunk (WireStateChunk, stateBody, stateType))
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
    , set4 = Nothing
    , opt5 = Nothing
    , nst6 = Nothing
    }

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000

state1expect :: ByteStringL
state1expect = [s|
    *set    #B/0000000DrW+r3pl1c4           !
                                    @`}KUW  >int1   275
                                    @}OUW   >nst6
                                    @}WUW   >opt5
                                    @}lUW   >set4
                                    @{10UW  >str2   >B/0000001GUW+r3pl1c4
                                    @}LcW   >str3   '190'

    *rga    #{1GUW                  @0      !
                                    @`]g6   '2'
                                    @)7     '7'
                                    @)8     '5'
    .
    |]

state4expect :: ByteStringL
state4expect = [s|
    *set    #B/0000000DrW+r3pl1c4                   !
                                    @`}WUW          >opt5
                                    @{10UW          >str2 >B/0000001GUW+r3pl1c4
                                    @}ZdW           >int1 166
                                    @}k2W   :`{0KUW >int1 275
                                    @{2OUW  :0      >str3 '206'
                                    @}WUW   :`{1LcW >str3 '190'
                                    @}lUW   :0      >set4 >B/00000030UW+r3pl1c4
                                    @{3GUW  :`{0lUW >set4
                                    @{5V2W  :0      >nst6 >B/0000005YMW+r3pl1c4
                                    @{6lUW  :`{0OUW >nst6

    *rga    #{1GUW                  @0      :0      !
                                    @`]g6   :`}nMW  '2'
                                    @)7     :{21UW  '7'
                                    @{2AUW  :0      '1'
                                    @}KUW           '4'
                                    @`]g8           '5'

    *set    #{30UW                  @0              !
                                    @`{5JdW         >}WUW

            #}WUW                   @0              !
                                    @`}lUW          >int1 135
                                    @{40UW          >nst6
                                    @}GUW           >opt5
                                    @}WUW           >set4
                                    @}lUW           >str2 >B/00000050UW+r3pl1c4
                                    @{55cW          >str3 '137'

    *rga    #{50UW                  @0              !
                                    @`]g6           '1'
                                    @)7             '3'
                                    @)8             '6'

    *set    #}YMW                   @0              !
                                    @`}mUW          >int1 138
                                    @}vUW           >nst6
                                    @{64UW          >opt5
                                    @}8UW           >set4
                                    @}GUW           >str2
                                    @}WUW           >str3
    .
    |]

example4expect :: StructSet13
example4expect = StructSet13
    { int1 = Just 166
    , str2 = Just $ RGA "145"
    , str3 = Just "206"
    , set4 = Just $
        ORSet [def{int1 = Just 135, str2 = Just $ RGA "136", str3 = Just "137"}]
    , opt5 = Nothing
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
    example3 <- evalEither $ evalObjectState state2 readObject
    example0 === example3

    -- apply operations to the object (frame)
    state4 <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        execObjectState state2 $ do
            checkCausality
            int1_set 166  -- plain field
            checkCausality
            str2_zoom $ RGA.edit "145"
            checkCausality
            do  value <- str3_read
                value === Just "190"
            str3_set "206"
            checkCausality
            Nothing <- set4_get
            set4_set $ ORSet []
            Just set4ref1 <- set4_get
            set4_zoom $ do
                ORSet.addValue
                    def { int1 = Just 135
                        , str2 = Just $ RGA "136"
                        , str3 = Just "137"
                        }
                checkCausality
            Just set4ref2 <- set4_get
            set4ref1 === set4ref2
            checkCausality
            do  value <- opt5_read
                value === Nothing
            do  value <- nst6_read
                value === Nothing
            nst6_set def{int1 = Just 138}
            checkCausality

    -- decode object after modification
    example4 <- evalEither $ evalObjectState state4 readObject
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
    checkObject root self WireStateChunk{stateType, stateBody} =
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
