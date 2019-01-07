{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}

module Gen where

import           Data.Text (Text)
import           Data.Word (Word64)
import           Hedgehog (MonadGen)
import           Hedgehog.Gen (choice, double, enumBounded, integral, list,
                               text, unicodeAll, word64, word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           RON.Event (CalendarTime (CalendarTime), Event (Event),
                            LocalTime (TCalendar, TEpoch, TLogical, TUnknown),
                            ReplicaId (ReplicaId), days, hours, minutes, months,
                            nanosecHundreds, seconds)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid), Op (Op),
                            RawOp (RawOp), StateChunk (StateChunk), StateFrame,
                            UUID, WireChunk (Query, Raw, Value), WireFrame,
                            WireReducedChunk (WireReducedChunk))
import           RON.Util.Word (Word60, leastSignificant2, leastSignificant4,
                                ls12, ls24, ls6, ls60)
import           RON.UUID (UuidFields (UuidFields))
import qualified RON.UUID as UUID

word60 :: MonadGen gen => gen Word60
word60 =
    ls60 <$> word64 (Range.exponential 0 $ 2 ^ (60 :: Int) - 1)

word64' :: MonadGen gen => gen Word64
word64' = word64 Range.linearBounded

uuid :: MonadGen gen => gen UUID
uuid = UUID.build <$> uuidFields

uuidFields :: MonadGen gen => gen UuidFields
uuidFields = UuidFields
    <$> (leastSignificant4 <$> word8 (Range.constant 0 15))
    <*> word60
    <*> (leastSignificant2 <$> word8 (Range.constant 0 3))
    <*> (leastSignificant2 <$> word8 (Range.constant 0 3))
    <*> choice [pure $ ls60 42, word60]

rawOp :: MonadGen gen => Int -> gen RawOp
rawOp size = RawOp <$> uuid <*> uuid <*> reducedOp size

reducedOp :: MonadGen gen => Int -> gen Op
reducedOp size = Op <$> uuid <*> uuid <*> payload size

wireChunk :: MonadGen gen => Int -> gen WireChunk
wireChunk size =
    choice [Raw <$> rawOp size, Value <$> rchunk size, Query <$> rchunk size]

rchunk :: MonadGen gen => Int -> gen WireReducedChunk
rchunk size = WireReducedChunk
    <$> rawOp size
    <*> list (Range.exponential 0 size) (reducedOp size)

wireFrame :: MonadGen gen => Int -> gen WireFrame
wireFrame size = list (Range.exponential 0 size) (wireChunk size)

stateFrame :: MonadGen gen => Int -> gen StateFrame
stateFrame size
    = Gen.map (Range.exponential 0 size)
    $ (,) <$> ((,) <$> uuid <*> uuid) <*> stateChunk size

stateChunk :: MonadGen gen => Int -> gen StateChunk
stateChunk size = StateChunk
    <$> choice [pure UUID.zero, uuid]
    <*> choice
        [ pure [Op UUID.zero UUID.zero []]
        , list (Range.exponential 0 size) $ reducedOp size
        ]

wireFrames :: MonadGen gen => Int -> gen [WireFrame]
wireFrames size = list (Range.exponential 0 size) (wireFrame size)

calendarTime :: MonadGen gen => gen CalendarTime
calendarTime = do
    months          <- ls12 <$> integral (Range.constant 0 4095)
    days            <- ls6  <$> integral (Range.constant 0 30)
    hours           <- ls6  <$> integral (Range.constant 0 23)
    minutes         <- ls6  <$> integral (Range.constant 0 59)
    seconds         <- ls6  <$> integral (Range.constant 0 59)
    nanosecHundreds <- ls24 <$> integral (Range.constant 0 10000000)
    pure CalendarTime{..}

eventTime :: MonadGen gen => gen LocalTime
eventTime = choice
    [ TCalendar <$> calendarTime
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
    , AString   <$> text (Range.exponential 0 size) unicodeAll
    , AUuid     <$> uuid
    ]

event :: MonadGen gen => gen Event
event = Event <$> eventTime <*> replicaId

replicaId :: MonadGen gen => gen ReplicaId
replicaId = ReplicaId <$> enumBounded <*> word60

shortText :: MonadGen gen => gen Text
shortText = text (Range.linear 0 10) unicodeAll
