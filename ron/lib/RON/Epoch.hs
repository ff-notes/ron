{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module RON.Epoch (
    EpochClock (..),
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

newtype EpochClock a = EpochClock (ReaderT (ReplicaId, IORef EpochTime) IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

instance ReplicaClock EpochClock where
    getPid = EpochClock $ reader fst

    advance time = EpochClock $ ReaderT $ \(_pid, timeVar) ->
        atomicModifyIORef' timeVar $ \t0 -> (max time t0, ())

    getEvents n0 = EpochClock $ ReaderT $ \(pid, timeVar) -> do
        let n = max n0 $ ls60 1
        realTime <- getCurrentEpochTime
        timeRangeStart <- atomicModifyIORef' timeVar $ \timeCur ->
            let timeRangeStart = max realTime $ succ timeCur
            in (timeRangeStart `word60add` pred n, timeRangeStart)
        pure
            [ EpochEvent t pid
            | t <- [timeRangeStart .. timeRangeStart `word60add` pred n]
            ]

runEpochClock :: ReplicaId -> IORef EpochTime -> EpochClock a -> IO a
runEpochClock replicaId timeVar (EpochClock action) =
    runReaderT action (replicaId, timeVar)

runEpochClockFromCurrentTime :: ReplicaId -> EpochClock a -> IO a
runEpochClockFromCurrentTime replicaId clock = do
    time <- getCurrentEpochTime
    timeVar <- newIORef time
    runEpochClock replicaId timeVar clock

getCurrentEpochTime :: IO EpochTime
getCurrentEpochTime
    =   epochTimeFromUnix @Word64
    .   round
    .   (* 10000000)
    <$> getPOSIXTime

-- | 'EpochTime' from Unix time in hundreds of milliseconds
epochTimeFromUnix :: Integral int => int -> EpochTime
epochTimeFromUnix
    =   leastSignificant60
    .   (+ 0x01B21DD213814000)
        -- the difference between Unix epoch and UUID epoch;
        -- the constant is taken from RFC 4122

localEpochTimeFromUnix :: Integral int => int -> LocalTime
localEpochTimeFromUnix = TEpoch . epochTimeFromUnix
