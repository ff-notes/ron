{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Data.Text
import           Debug.Pretty.Simple
import           Text.Pretty.Simple

import           RON.Data
import           RON.Data.ORSet as ORSet
import           RON.Event
import           RON.Event.Simulation

main :: IO ()
main =
    pPrint $ runExcept $ runNetworkSimT $ do
        state1 <-
            runReplicaSimT (mkReplica ApplicationSpecific 1) $
                newObjectFrame $ ORSet []
        pTraceShowM state1
        state2 <-
            runReplicaSimT (mkReplica ApplicationSpecific 2) $
                execObjectState state1 $ ORSet.addValue $ ORSet ["ab" :: Text]
        pTraceShowM state2
