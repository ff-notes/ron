{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}

module Gen where

import           Data.Word (Word64)
import           Hedgehog (MonadGen)
import           Hedgehog.Gen (choice, double, enumBounded, integral, list,
                               text, unicode, word64)
import qualified Hedgehog.Range as Range

import           RON.Event (Calendar (..), Event (..),
                            LocalTime (TCalendar, TEpoch, TLogical, TUnknown),
                            ReplicaId (..))
import           RON.Internal.Word (Word60, leastSignificant60, ls12, ls24, ls6)
import           RON.Types (Atom (..), Chunk (..), Op (..), RChunk (..),
                            RawFrame, RawOp (..), UUID (..))

word64' :: MonadGen gen => gen Word64
word64' = word64 Range.linearBounded

word60 :: MonadGen gen => gen Word60
word60 = leastSignificant60 <$> word64'

uuid :: MonadGen gen => gen UUID
uuid = UUID <$> word64' <*> word64'

rawOp :: MonadGen gen => Int -> gen RawOp
rawOp size = RawOp <$> uuid <*> uuid <*> reducedOp size

reducedOp :: MonadGen gen => Int -> gen Op
reducedOp size = Op <$> uuid <*> uuid <*> payload size

chunk :: MonadGen gen => Int -> gen Chunk
chunk size =
    choice [Raw <$> rawOp size, Value <$> rchunk size, Query <$> rchunk size]

rchunk :: MonadGen gen => Int -> gen RChunk
rchunk size = RChunk
    <$> rawOp size
    <*> list (Range.exponential 0 size) (reducedOp size)

frame :: MonadGen gen => Int -> gen RawFrame
frame size = list (Range.exponential 0 size) (chunk size)

frames :: MonadGen gen => Int -> gen [RawFrame]
frames size = list (Range.exponential 0 size) (frame size)

calendar :: MonadGen gen => gen Calendar
calendar = do
    months          <- ls12 <$> integral (Range.constant 0 4095)
    days            <- ls6  <$> integral (Range.constant 0 30)
    hours           <- ls6  <$> integral (Range.constant 0 23)
    minutes         <- ls6  <$> integral (Range.constant 0 59)
    seconds         <- ls6  <$> integral (Range.constant 0 59)
    nanosecHundreds <- ls24 <$> integral (Range.constant 0 10000000)
    pure Calendar{..}

eventTime :: MonadGen gen => gen LocalTime
eventTime = choice
    [ TCalendar <$> calendar
    , TLogical  <$> word60
    , TEpoch    <$> word60
    , TUnknown  <$> word60
    ]

payload :: MonadGen gen => Int -> gen [Atom]
payload size = list (Range.exponential 0 size) (atom size)

atom :: MonadGen gen => Int -> gen Atom
atom size = choice
    [ AFloat    <$> double (Range.exponentialFloatFrom 0 (-1e308) 1e308)
    , AInteger  <$> integral (Range.exponentialFrom 0 (-1e18) 1e18)
    , AString   <$> text (Range.exponential 0 size) unicode
    , AUuid     <$> uuid
    ]

event :: MonadGen gen => gen Event
event = Event <$> eventTime <*> replicaId

replicaId :: MonadGen gen => gen ReplicaId
replicaId = ReplicaId <$> enumBounded <*> word60
