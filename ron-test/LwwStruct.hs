{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Hedgehog (Property, evalEither, evalExceptT, property, (===))

import           RON.Data (evalObjectState, execObjectState, newObjectFrame,
                           readObject)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import           RON.Text (parseObject, serializeObject)
import           RON.Util (ByteStringL)

import           LwwStruct.Types (Struct51 (..), int1_set, nst5_read, opt6_read,
                                  opt6_set, set4_zoom, str2_zoom, str3_read,
                                  str3_set)
import           Orphans ()
import           String (s)

example0 :: Struct51
example0 = Struct51
    { int1 = Just 275
    , str2 = Just $ RGA "275"
    , str3 = Just "190"
    , set4 = Just $ ORSet []
    , nst5 = Nothing
    , opt6 = Just 74
    }

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000

ex1expect :: ByteStringL
ex1expect = [s|
    *lww    #B/0000000DrW+r3pl1c4                   !
                                    @`      :int1   275
                                            :nst5
                                            :opt6   74
                                            :set4   >}KUW
                                            :str2   >}OUW
                                            :str3   '190'

    *set    #}KUW                   @0      :0      !

    *rga    #}OUW                                   !
                                    @`}Wg6          '2'
                                    @)7             '7'
                                    @)8             '5'
    .
    |]

ex4expect :: ByteStringL
ex4expect = [s|
    *lww    #B/0000000DrW+r3pl1c4                   !
                                    @`}WUW  :int1   166
                                    @`      :nst5
                                    @{23dW  :opt6
                                    @`      :set4   >}KUW
                                            :str2   >}OUW
                                    @{1HUW  :str3   '206'

    *set    #}KUW                   @0      :0      !
                                    @`{1qcW         >{1QUW

    *rga    #}OUW                   @0              !
                                    @`}Wg6  :`}acW  '2'
                                    @)7     :}odW   '7'
                                    @}~2W   :0      '1'
                                    @{12MW          '4'
                                    @{0Wg8          '5'

    *lww    #{1QUW                  @0              !
                                    @`      :int1   135
                                            :nst5
                                            :opt6
                                            :set4   >}_UW
                                            :str2   >}dUW
                                            :str3   '137'

    *set    #}_UW                   @0      :0      !

    *rga    #}dUW                                   !
                                    @`}lg6          '1'
                                    @)7             '3'
                                    @)8             '6'
    .
    |]

example4expect :: Struct51
example4expect = Struct51
    { int1 = Just 166
    , str2 = Just $ RGA "145"
    , str3 = Just "206"
    , set4 = Just $ ORSet
        [Struct51
            { int1 = Just 135
            , str2 = Just $ RGA "136"
            , str3 = Just "137"
            , set4 = Just $ ORSet []
            , nst5 = Nothing
            , opt6 = Nothing
            }]
    , nst5 = Nothing
    , opt6 = Nothing
    }

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    -- create an object
    ex1state <-
        runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example0
    let (oid, ex1ser) = serializeObject ex1state
    prep ex1expect === prep ex1ser

    -- parse newly created object
    ex2state <- evalEither $ parseObject oid ex1ser
    ex1state === ex2state

    -- decode newly created object
    example3 <- evalEither $ evalObjectState ex2state readObject
    example0 === example3

    -- apply operations to the object (frame)
    ex4state <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        execObjectState ex2state $ do
            -- plain field
            int1_set $ Just 166
            str2_zoom $ RGA.edit "145"
            str3Value <- str3_read
            str3Value === Just "190"
            str3_set $ Just "206"
            set4_zoom $
                ORSet.addValue
                    Struct51
                        { int1 = Just 135
                        , str2 = Just $ RGA "136"
                        , str3 = Just "137"
                        , set4 = Just $ ORSet []
                        , nst5 = Nothing
                        , opt6 = Nothing
                        }
            nst5Value <- nst5_read
            nst5Value === Nothing
            opt6Value <- opt6_read
            opt6Value === Just 74
            opt6_set Nothing

    -- decode object after modification
    example4 <- evalEither $ evalObjectState ex4state readObject
    example4expect === example4

    -- serialize object after modification
    prep ex4expect === prep (snd $ serializeObject ex4state)
    parseObject oid ex4expect === Right ex4state

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines
