{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module RON.Epoch (
    EpochClock,
    getCurrentEpochTime,
    localEpochTimeFromUnix,
    runEpochClock,
    runEpochClockFromCurrentTime,
) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT (ReaderT), reader, runReaderT)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word (Word64)

import           RON.Event (EpochEvent (EpochEvent), EpochTime,
                            LocalTime (TEpoch), ReplicaClock, ReplicaId,
                            advance, getEvents, getPid)
import           RON.Internal.Word (leastSignificant60, ls60, word60add)

-- | Real epoch clock.
-- Uses kind of global variable to ensure strict monotonicity.
newtype EpochClock a = EpochClock (ReaderT (ReplicaId, IORef EpochTime) IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

instance ReplicaClock EpochClock where
    getPid = EpochClock $ reader fst

    advance time = EpochClock $ ReaderT $ \(_pid, timeVar) ->
        atomicModifyIORef' timeVar $ \t0 -> (max time t0, ())

    getEvents n0 = EpochClock $ ReaderT $ \(pid, timeVar) -> do
        let n = max n0 $ ls60 1
        realTime <- getCurrentEpochTime
        (begin, end) <- atomicModifyIORef' timeVar $ \timeCur -> let
            begin = max realTime $ succ timeCur
            end   = begin `word60add` pred n
            in (end, (begin, end))
        pure [EpochEvent t pid | t <- [begin .. end]]

-- | Run 'EpochClock' action with explicit time variable.
runEpochClock :: ReplicaId -> IORef EpochTime -> EpochClock a -> IO a
runEpochClock replicaId timeVar (EpochClock action) =
    runReaderT action (replicaId, timeVar)

-- | Like 'runEpochClock', but initialize time variable with current wall time.
runEpochClockFromCurrentTime :: ReplicaId -> EpochClock a -> IO a
runEpochClockFromCurrentTime replicaId clock = do
    time <- getCurrentEpochTime
    timeVar <- newIORef time
    runEpochClock replicaId timeVar clock

-- | Get current time in 'EpochTime' format (with 100 ns resolution).
-- Monotonicity is not guaranteed.
getCurrentEpochTime :: IO EpochTime
getCurrentEpochTime
    =   epochTimeFromUnix @Word64
    .   round
    .   (* 10000000)
    <$> getPOSIXTime

-- | Convert unix time in hundreds of milliseconds to RFC 4122 time.
epochTimeFromUnix :: Integral int => int -> EpochTime
epochTimeFromUnix
    =   leastSignificant60
    .   (+ 0x01B21DD213814000)
        -- the difference between Unix epoch and UUID epoch;
        -- the constant is taken from RFC 4122

-- | Convert unix time in hundreds of milliseconds to RFC 4122 time.
localEpochTimeFromUnix :: Integral int => int -> LocalTime
localEpochTimeFromUnix = TEpoch . epochTimeFromUnix
