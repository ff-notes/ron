{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Lamport clock network simulation.
-- 'ReplicaSim' provides 'Replica' and 'Clock' instances,
-- replicas may interchange data while they are connected in a 'NetworkSim'.
module RON.Event.Simulation (
    NetworkSimT,
    ReplicaSimT,
    runNetworkSimT,
    runReplicaSimT,
) where

import           RON.Prelude

import           Control.Monad.State.Strict (state)
import qualified Data.HashMap.Strict as HM

import           RON.Event (Event (..), Replica (Replica), ReplicaClock,
                            TimeVariety (Logical), advance, getEvents, getPid,
                            mkTime)
import           RON.Util.Word (Word60, ls60)

-- | Lamport clock simulation. Key is 'Replica'.
-- Non-present value is equivalent to (0, initial).
newtype NetworkSimT m a = NetworkSim (StateT (HashMap Replica Word60) m a)
    deriving (Applicative, Functor, Monad, MonadError e)

instance MonadTrans NetworkSimT where
    lift = NetworkSim . lift

-- | ReplicaSim inside Lamport clock simulation.
newtype ReplicaSimT m a = ReplicaSim (ReaderT Replica (NetworkSimT m) a)
    deriving (Applicative, Functor, Monad, MonadError e)

instance MonadTrans ReplicaSimT where
    lift = ReplicaSim . lift . lift

instance Monad m => ReplicaClock (ReplicaSimT m) where
    getPid = ReplicaSim ask

    getEvents n' = ReplicaSim $ do
        replica <- ask
        (t0, t1) <-
            lift $ NetworkSim $ state $ \replicaStates -> let
                t0orig = HM.lookupDefault (ls60 0) replica replicaStates
                Replica r = replica
                randomLeap =
                    ls60 . fromIntegral $ hash (t0orig, n, r) `mod` 0x10000
                t0 = t0orig + randomLeap
                t1 = t0 + n
                in ((t0, t1), HM.insert replica t1 replicaStates)
        pure [Event{time = mkTime Logical t, replica} | t <- [succ t0 .. t1]]
      where
        n = max n' (ls60 1)

    advance time = ReplicaSim $ do
        rid <- ask
        lift . NetworkSim . modify' $ HM.alter (Just . advancePS) rid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

instance MonadFail m => MonadFail (ReplicaSimT m) where
    fail = lift . fail

-- | Execute network simulation
--
-- Usage:
--
-- @
-- 'runExceptT' . runNetworkSimT $ do
--     'runReplicaSimT' r1 $ do
--         actions...
--     'runReplicaSimT' r2 $ do
--         actions...
--     'runReplicaSimT' r1 $ ...
-- @
--
-- Each 'runNetworkSimT' starts its own networks.
-- One shouldn't use in one network events generated in another.
runNetworkSimT :: Monad m => NetworkSimT m a -> m a
runNetworkSimT (NetworkSim action) = evalStateT action mempty

runReplicaSimT :: Replica -> ReplicaSimT m a -> NetworkSimT m a
runReplicaSimT rid (ReplicaSim action) = runReaderT action rid
