{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module RON.Epoch (
    EpochClock,
    EpochClockT,
    decode,
    encode,
    epochTimeFromUnix,
    getCurrentEpochTime,
    runEpochClock,
    runEpochClockFromCurrentTime,
) where

import           RON.Prelude

import           Data.IORef (atomicModifyIORef', newIORef)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime,
                                        posixSecondsToUTCTime)

import           RON.Event (Event (..), Replica, ReplicaClock,
                            TimeVariety (Epoch), advance, getEvents, getPid,
                            mkTime)
import           RON.Util.Word (Word60, leastSignificant60, safeCast)

-- | Real epoch clock.
-- Uses kind of global variable to ensure strict monotonicity.
newtype EpochClockT m a = EpochClock (ReaderT (Replica, IORef Word60) m a)
    deriving (Applicative, Functor, Monad, MonadIO)

type EpochClock = EpochClockT IO

instance MonadIO m => ReplicaClock (EpochClockT m) where
    getPid = EpochClock $ reader fst

    advance theirTime =
        EpochClock $
        ReaderT $ \(_pid, timeVar) ->
            liftIO $
            atomicModifyIORef' timeVar $ \ourTime -> (max theirTime ourTime, ())

    getEvents n0 =
        EpochClock $
        ReaderT $ \(pid, timeVar) -> do
            let n = max n0 1
            realTime <- liftIO getCurrentEpochTime
            (begin, end) <-
                liftIO $
                atomicModifyIORef' timeVar $ \timeCur -> let
                    begin = max realTime $ succ timeCur
                    end   = begin + pred n
                    in (end, (begin, end))
            pure
                [ Event{time = mkTime Epoch t, replica = pid}
                | t <- [begin .. end]
                ]

instance MonadTrans EpochClockT where
    lift = EpochClock . lift @(ReaderT _)

-- | Run 'EpochClock'/'EpochClockT' action with explicit time variable.
runEpochClock :: Replica -> IORef Word60 -> EpochClockT m a -> m a
runEpochClock replicaId timeVar (EpochClock action) =
    runReaderT action (replicaId, timeVar)

-- | Like 'runEpochClock', but initialize time variable with current wall time.
runEpochClockFromCurrentTime :: Replica -> EpochClock a -> IO a
runEpochClockFromCurrentTime replicaId clock = do
    wallTime <- getCurrentEpochTime
    timeVar <- newIORef wallTime
    runEpochClock replicaId timeVar clock

-- | Get current time in 'Time' format (with 100 ns resolution).
-- Monotonicity is not guaranteed.
getCurrentEpochTime :: MonadIO m => m Word60
getCurrentEpochTime = liftIO $ encode <$> getPOSIXTime

-- | Convert unix time in hundreds of milliseconds to RFC 4122 time.
epochTimeFromUnix :: Word64 -> Word60
epochTimeFromUnix = leastSignificant60 . (+ epochDiff)

-- The difference between Unix epoch and UUID epoch;
-- the constant is taken from RFC 4122
epochDiff :: Word64
epochDiff = 0x01B21DD213814000

-- | Decode date and time from UUID epoch timestamp
decode :: Word60 -> UTCTime
decode
    = posixSecondsToUTCTime
    . realToFrac
    . (% 10000000)
    . subtract epochDiff
    . safeCast

encode :: POSIXTime -> Word60
encode = epochTimeFromUnix . round . (* 10000000)
