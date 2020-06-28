{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RON.Epoch (
    EpochClock,
    decode,
    encode,
    getCurrentEpochTime,
    localEpochTimeFromUnix,
    runEpochClock,
    runEpochClockFromCurrentTime,
) where

import           RON.Prelude

import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime,
                                        posixSecondsToUTCTime)

import           RON.Event (EpochEvent (EpochEvent), EpochTime,
                            LocalTime (TEpoch), ReplicaClock, ReplicaId,
                            advance, getEvents, getPid)
import           RON.Util.Word (leastSignificant60, safeCast)

-- | Real epoch clock.
-- Uses kind of global variable to ensure strict monotonicity.
newtype EpochClock a = EpochClock (ReaderT (ReplicaId, IORef EpochTime) IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

instance ReplicaClock EpochClock where
    getPid = EpochClock $ reader fst

    advance time = EpochClock $ ReaderT $ \(_pid, timeVar) ->
        atomicModifyIORef' timeVar $ \t0 -> (max time t0, ())

    getEvents n0 = EpochClock $ ReaderT $ \(pid, timeVar) -> do
        let n = max n0 1
        realTime <- getCurrentEpochTime
        (begin, end) <- atomicModifyIORef' timeVar $ \timeCur -> let
            begin = max realTime $ succ timeCur
            end   = begin + pred n
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
getCurrentEpochTime = encode <$> getPOSIXTime

-- | Convert unix time in hundreds of milliseconds to RFC 4122 time.
epochTimeFromUnix :: Word64 -> EpochTime
epochTimeFromUnix = leastSignificant60 . (+ epochDiff)

-- The difference between Unix epoch and UUID epoch;
-- the constant is taken from RFC 4122
epochDiff :: Word64
epochDiff = 0x01B21DD213814000

-- | Convert unix time in hundreds of milliseconds to RFC 4122 time.
localEpochTimeFromUnix :: Word64 -> LocalTime
localEpochTimeFromUnix = TEpoch . epochTimeFromUnix

-- | Decode date and time from UUID epoch timestamp
decode :: EpochTime -> UTCTime
decode
    = posixSecondsToUTCTime
    . realToFrac
    . (% 10000000)
    . subtract epochDiff
    . safeCast

encode :: POSIXTime -> EpochTime
encode = epochTimeFromUnix . round . (* 10000000)
