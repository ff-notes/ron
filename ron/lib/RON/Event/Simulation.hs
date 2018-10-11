{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Lamport clock simulation
module RON.Event.Simulation
    ( NetworkSim
    , NetworkSimT (..)
    , ReplicaSim
    , ReplicaSimT (..)
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
import           Data.Maybe (fromMaybe)

import           RON.Event (Clock, EpochEvent (..), Replica, ReplicaId (..),
                            advance, getEvents, getPid)
import           RON.Internal.Word (Word60, Word64, ls60, word60add)

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

instance Monad m => Replica (ReplicaSimT m) where
    getPid = ReplicaSim ask

instance Monad m => Clock (ReplicaSimT m) where
    getEvents n' = ReplicaSim $ do
        rid <- ask
        t0 <- lift $ preIncreaseTime rid
        pure [EpochEvent t rid | t <- [t0 .. t0 `word60add` n]]
      where
        n = max n' (ls60 (1 :: Word64))

    advance time = ReplicaSim $ do
        rid <- ask
        lift . NetworkSim . modify $ HM.alter (Just . advancePS) rid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

runNetworkSim :: NetworkSim a -> a
runNetworkSim (NetworkSim action) = evalState action mempty

runNetworkSimT :: Monad m => NetworkSimT m a -> m a
runNetworkSimT (NetworkSim action) = evalStateT action mempty

runReplicaSim :: ReplicaId -> ReplicaSim a -> NetworkSim a
runReplicaSim rid (ReplicaSim action) = runReaderT action rid

runReplicaSimT :: ReplicaId -> ReplicaSimT m a -> NetworkSimT m a
runReplicaSimT rid (ReplicaSim action) = runReaderT action rid

-- | Increase time by rid and return new value
preIncreaseTime :: Monad m => ReplicaId -> NetworkSimT m Word60
preIncreaseTime rid = NetworkSim $ state $ \pss ->
    let time = succ $ fromMaybe (ls60 0) $ HM.lookup rid pss
    in  (time, HM.insert rid time pss)
