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

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, evalState, evalStateT,
                                             modify, state)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Functor.Identity (Identity)
import           Data.Hashable (hash)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)

import           RON.Event (Clock, EpochEvent (..), Replica, ReplicaId (..),
                            advance, getEvents, getPid)
import           RON.Internal.Word (Word60, Word64, leastSignificant60,
                                    word60add)

-- | Lamport clock simulation. Key is 'ReplicaId'.
-- Non-present value is equivalent to (0, initial).
newtype NetworkSimT m a =
    NetworkSim (ExceptT String (StateT (HashMap ReplicaId Word60) m) a)
    deriving (Applicative, Functor, Monad, MonadError String)

instance MonadTrans NetworkSimT where
    lift = NetworkSim . lift . lift

instance Monad m => MonadFail (NetworkSimT m) where
    fail = throwError

type NetworkSim = NetworkSimT Identity

-- | ReplicaSim inside Lamport clock simulation.
newtype ReplicaSimT m a = ReplicaSim (ReaderT ReplicaId (NetworkSimT m) a)
    deriving (Applicative, Functor, Monad, MonadFail)

type ReplicaSim = ReplicaSimT Identity

instance MonadTrans ReplicaSimT where
    lift = ReplicaSim . lift . lift

instance Monad m => Replica (ReplicaSimT m) where
    getPid = ReplicaSim ask

instance Monad m => Clock (ReplicaSimT m) where
    getEvents n' = ReplicaSim $ do
        rid <- ask
        t0 <- lift $ preIncreaseTime n rid
        pure [EpochEvent t rid | t <- [t0 .. t0 `word60add` n]]
      where
        n = max n' (leastSignificant60 (1 :: Word64))

    advance time = ReplicaSim $ do
        rid <- ask
        lift . NetworkSim . modify $ HM.alter (Just . advancePS) rid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

runNetworkSim :: NetworkSim a -> Either String a
runNetworkSim (NetworkSim action) =
    evalState (runExceptT action) mempty

runNetworkSimT :: Monad m => NetworkSimT m a -> m (Either String a)
runNetworkSimT (NetworkSim action) =
    evalStateT (runExceptT action) mempty

runReplicaSim :: ReplicaId -> ReplicaSim a -> NetworkSim a
runReplicaSim rid (ReplicaSim action) = runReaderT action rid

runReplicaSimT :: ReplicaId -> ReplicaSimT m a -> NetworkSimT m a
runReplicaSimT rid (ReplicaSim action) = runReaderT action rid

-- | Increase time by rid and return new value
preIncreaseTime :: Monad m => Word60 -> ReplicaId -> NetworkSimT m Word60
preIncreaseTime n rid = NetworkSim $ state $ \pss ->
    let time0 =
            fromMaybe (leastSignificant60 (0 :: Word64) :: Word60) $
            HM.lookup rid pss
        ReplicaId _ r = rid
        d     = leastSignificant60 $ hash (time0, n, r) `mod` 0x100
        time  = max (succ time0) (time0 `word60add` d)
    in  (time, HM.insert rid time pss)
