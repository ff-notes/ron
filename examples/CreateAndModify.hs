{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Data.Text
import           Debug.Trace

import           RON.Data
import           RON.Data.ORSet as ORSet
import           RON.Event
import           RON.Event.Simulation

main :: IO ()
main =
    print $ runExcept $ runNetworkSimT $ do
        state1 <-
            runReplicaSimT (applicationSpecific 1) $ newObjectFrame $ ORSet []
        traceShowM state1
        state2 <-
            runReplicaSimT (applicationSpecific 2) $ execObjectState state1 $
            ORSet.addValue $ ORSet ["ab" :: Text]
        traceShowM state2
