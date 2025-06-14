{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LwwStruct (prop_lwwStruct) where

import RON.Prelude

import Data.ByteString.Lazy.Char8 qualified as BSLC
import Hedgehog (Property, evalEither, evalExceptT, property, (===))

import RON.Data (evalObjectState, execObjectState, newObjectFrame, readObject)
import RON.Data.ORSet (ORSet (ORSet))
import RON.Data.ORSet qualified as ORSet
import RON.Data.RGA (RGA (RGA))
import RON.Data.RGA qualified as RGA
import RON.Event (OriginVariety (ApplicationSpecific), Replica, mkReplica)
import RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import RON.Text (parseObject, serializeObject)

import LwwStruct.Types (
    Struct51 (..),
    int1_set,
    nst5_read,
    nst5_set,
    set4_zoom,
    str2_zoom,
    str3_read,
    str3_set,
 )
import Orphans ()
import String (s)

example0 :: Struct51
example0 =
    Struct51
        { int1 = Just 275
        , str2 = Just $ RGA "275"
        , str3 = Just "190"
        , set4 = Just $ ORSet []
        , nst5 = Nothing
        }

-- | "r3pl1c4"
replica :: Replica
replica = mkReplica ApplicationSpecific 0xd83d30067100000

ex1expect :: ByteStringL
ex1expect =
    [s|
        *lww    #7/0000000DrW+r3pl1c4                   !
                                        @`      :int1   275
                                                :nst5
                                                :set4   >}KUW
                                                :str2   >}OUW
                                                :str3   '190'

        *set    #}KUW                   @0      :0      !

        *rga    #}OUW                                   !
                                        @`}WUW          '2'
                                        @)X             '7'
                                        @)Y             '5'
        .
    |]

ex4expect :: ByteStringL
ex4expect =
    [s|
        *lww    #7/0000000DrW+r3pl1c4                   !
                                        @`}WUW  :int1   166
                                        @{1p0W  :nst5
                                        @`      :set4   >}KUW
                                                :str2   >}OUW
                                        @{19UW  :str3   '206'

        *set    #}KUW                   @0      :0      !
                                        @`{1kmp         >{1AUW

        *rga    #}OUW                   @0              !
                                        @`}WUW  :`}kmp  '2'
                                        @)X     :}p0W   '7'
                                        @{112W  :0      '1'
                                        @}6MW           '4'
                                        @{0WUY          '5'

        *lww    #{1AUW                  @0              !
                                        @`      :int1   135
                                                :nst5
                                                :set4   >}KUW
                                                :str2   >}OUW
                                                :str3   '137'

        *set    #}KUW                   @0      :0      !

        *rga    #}OUW                                   !
                                        @`}WUW          '1'
                                        @)X             '3'
                                        @)Y             '6'
        .
    |]

example4expect :: Struct51
example4expect =
    Struct51
        { int1 = Just 166
        , str2 = Just $ RGA "145"
        , str3 = Just "206"
        , set4 =
            Just $
                ORSet
                    [ Struct51
                        { int1 = Just 135
                        , str2 = Just $ RGA "136"
                        , str3 = Just "137"
                        , set4 = Just $ ORSet []
                        , nst5 = Nothing
                        }
                    ]
        , nst5 = Nothing
        }

prop_lwwStruct :: Property
prop_lwwStruct = property do
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
            runNetworkSimT $
                runReplicaSimT replica $
                    execObjectState ex2state do
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
                                    }
                        nst5Value <- nst5_read
                        nst5Value === Nothing
                        nst5_set Nothing

    -- decode object after modification
    example4 <- evalEither $ evalObjectState ex4state readObject
    example4expect === example4

    -- serialize object after modification
    prep ex4expect === prep (snd $ serializeObject ex4state)
    parseObject oid ex4expect === Right ex4state
  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines
