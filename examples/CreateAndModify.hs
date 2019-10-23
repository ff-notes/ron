{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Data.Text
import           Data.Typeable
import           Debug.Pretty.Simple
import           Text.Pretty.Simple

import           RON.Data
import           RON.Data.ORSet as ORSet
import           RON.Event
import           RON.Event.Simulation

main :: IO ()
main =
    pPrint $ runExcept $ runNetworkSimT $ do
        state1 <- runReplicaSimT (applicationSpecific 1) newEmptyDocFrame

        pTraceShowM $ typeOf state1
        {- DocFrame (ORSet (ObjectRef (ORSet Text))) -}

        pTraceShowM state1
        {-  DocFrame{
                uuid = "B/0000000DrV+0000000001",
                frame = fromList []
                }
            -}

        state2 <-
            runReplicaSimT (applicationSpecific 2) $
                execDocState state1 $ \set1 -> do
                    set2 <- newObject
                    ORSet.addValue ("ab" :: Text) set2
                    ORSet.addValue set2 set1

        pTraceShowM $ typeOf state2
        {- DocFrame (ORSet (ObjectRef (ORSet Text))) -}

        pTraceShowM state2
        {-  DocFrame{
                uuid = "B/0000000DrV+0000000001",
                frame = fromList [
                    (   "B/0000000DrU+0000000002",
                        WireStateChunk{
                            stateType = "set",
                            stateBody = [
                                Op{ opId = "B/0000000NCr+0000000002",
                                    refId = "0",
                                    payload = [AString "ab"]
                                    }
                                ]
                            }
                        ),
                    (   "B/0000000DrV+0000000001",
                        WireStateChunk{
                            stateType = "set",
                            stateBody = [
                                Op{ opId = "B/0000000Zmr+0000000002",
                                    refId = "0",
                                    payload = [AUuid "B/0000000DrU+0000000002"]
                                    }
                                ]
                            }
                        )
                    ]
                }
            -}

    -- Right ()
