{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Lamport clock network simulation.
-- 'ReplicaSim' provides 'Replica' and 'Clock' instances,
-- replicas may interchange data while they are connected in a 'NetworkSim'.
module RON.Event.Simulation
    ( NetworkSim
    , NetworkSimT
    , ReplicaSim
    , ReplicaSimT
    , runNetworkSim
    , runNetworkSimT
    , runReplicaSim
    , runReplicaSimT
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, evalState, evalStateT,
                                             modify, state)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Functor.Identity (Identity)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import           RON.Event (EpochEvent (EpochEvent), ReplicaClock, ReplicaId,
                            advance, getEvents, getPid)
import           RON.Internal.Word (Word60, ls60, word60add)

-- | Lamport clock simulation. Key is 'ReplicaId'.
-- Non-present value is equivalent to (0, initial).
newtype NetworkSimT m a = NetworkSim (StateT (HashMap ReplicaId Word60) m a)
    deriving (Applicative, Functor, Monad)

instance MonadTrans NetworkSimT where
    lift = NetworkSim . lift

type NetworkSim = NetworkSimT Identity

-- | ReplicaSim inside Lamport clock simulation.
newtype ReplicaSimT m a = ReplicaSim (ReaderT ReplicaId (NetworkSimT m) a)
    deriving (Applicative, Functor, Monad)

type ReplicaSim = ReplicaSimT Identity

instance MonadTrans ReplicaSimT where
    lift = ReplicaSim . lift . lift

instance Monad m => ReplicaClock (ReplicaSimT m) where
    getPid = ReplicaSim ask

    getEvents n' = ReplicaSim $ do
        rid <- ask
        (t0, t1) <-
            lift $ NetworkSim $ state $ \replicaStates -> let
                t0 = HM.lookupDefault (ls60 1) rid replicaStates
                t1 = t0 `word60add` n
                in ((t0, t1), HM.insert rid t1 replicaStates)
        pure [EpochEvent t rid | t <- [t0 .. t1]]
      where
        n = max n' (ls60 1)

    advance time = ReplicaSim $ do
        rid <- ask
        lift . NetworkSim . modify $ HM.alter (Just . advancePS) rid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

-- | Execute network simulation
--
-- Usage:
--
-- @
-- runNetworkSim $ do
--     'runReplicaSim' r1 $ do
--         actions...
--     'runReplicaSim' r2 $ do
--         actions...
--     'runReplicaSim' r1 $ ...
-- @
--
-- Each 'runNetworkSim' starts its own networks.
-- One shouldn't use in one network events generated in another.
runNetworkSim :: NetworkSim a -> a
runNetworkSim (NetworkSim action) = evalState action mempty

runNetworkSimT :: Monad m => NetworkSimT m a -> m a
runNetworkSimT (NetworkSim action) = evalStateT action mempty

runReplicaSim :: ReplicaId -> ReplicaSim a -> NetworkSim a
runReplicaSim rid (ReplicaSim action) = runReaderT action rid

runReplicaSimT :: ReplicaId -> ReplicaSimT m a -> NetworkSimT m a
runReplicaSimT rid (ReplicaSim action) = runReaderT action rid
