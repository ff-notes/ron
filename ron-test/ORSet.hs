{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ORSet (prop_orSet) where

import RON.Prelude

import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Hedgehog (Property, evalEither, evalExceptT, property, (===))

import RON.Data (evalObjectState, execObjectState, newObjectFrame, readObject)
import RON.Data.ORSet (
    ORSet (ORSet),
    addValue,
    findAnyAlive',
    removeValue,
    zoomItem,
 )
import RON.Event (OriginVariety (ApplicationSpecific), Replica, mkReplica)
import RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import RON.Schema.TH (mkReplicated)
import RON.Text (parseObject, serializeObject)

import Orphans ()
import String (s)

[mkReplicated|
    (alias Set27
        (ORSet                      ; top-level set of objects
            (ORSet                  ; nested set of object
                (ORSet String))))   ; nested set of atoms
|]

example0 :: Set27
example0 = ORSet [ORSet [ORSet ["octaves"]]]

state1expect :: BSL.ByteString
state1expect =
    [s|
    *set    #7/0000000WUW+r3pl1c4           !
                                    @`}OUW  'octaves'
            #}lUW                   @0      !
                                    @`}KUW  >}WUW
            #{10UW                  @0      !
                                    @`{0DrW >{0lUW
    .
    |]

example4expect :: Set27
example4expect = ORSet [ORSet [ORSet ["candies"]], ORSet []]

state4expect :: BSL.ByteString
state4expect =
    [s|
    *set    #7/0000000WUW+r3pl1c4                   !
                                    @`{1lUW         'candies'
                                    @{20UW  :`{0OUW 'octaves'
            #}lUW                   @0      :0      !
                                    @`}KUW          >}WUW
            #{10UW                  @0              !
                                    @`{0DrW         >{0lUW
                                    @{1WUW          >}GUW
            #}GUW                   @0              !
    .
    |]

prop_orSet :: Property
prop_orSet =
    ( property
        . evalExceptT
        . runNetworkSimT
        . runReplicaSimT replica
    )
        do
            -- create an object
            state1 <- newObjectFrame example0
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
                execObjectState state2 $ do
                    -- top-level
                    addValue $ ORSet []
                    set1 <- findAnyAlive'
                    zoomItem set1 $ do
                        set2 <- findAnyAlive'
                        zoomItem set2 $ do
                            addValue "candies"
                            removeValue "octaves"

            -- decode object after modification
            example4 <- evalEither $ evalObjectState state4 readObject
            example4expect === example4

            -- serialize object after modification
            prep state4expect === prep (snd $ serializeObject state4)
  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines

-- | "r3pl1c4"
replica :: Replica
replica = mkReplica ApplicationSpecific 0xd83d30067100000
