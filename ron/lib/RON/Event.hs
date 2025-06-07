{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Event (
    CalendarTime (..),
    Event (..),
    Time (..),
    TimeVariety (.., Calendar, Logical, Epoch, Unknown),
    OriginVariety (
        ..,
        TrieForked,
        CryptoForked,
        RecordForked,
        ApplicationSpecific
    ),
    ReplicaClock (..),
    Replica (..),
    advanceToUuid,
    decodeCalendar,
    decodeEvent,
    decodeReplica,
    encodeCalendar,
    encodeEvent,
    getEvent,
    getEventUuid,
    getEventUuids,
    mkCalendarDate,
    mkCalendarDateTime,
    mkCalendarDateTimeNano,
    mkCalendarEvent,
    mkReplica,
    mkTime,
    timeValue,
    timeVariety,
) where

import RON.Prelude

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Char8 qualified as BSC
import Data.Time (fromGregorianValid, makeTimeOfDayValid)
import Text.Show qualified

import RON.Base64 (encode60short)
import RON.UUID (UUID (..), UuidFields (..))
import RON.UUID qualified as UUID
import RON.Util.Word (
    Word12,
    Word2,
    Word24,
    Word6,
    Word60,
    leastSignificant12,
    leastSignificant2,
    leastSignificant24,
    leastSignificant6,
    ls12,
    ls24,
    ls6,
    ls60,
    safeCast,
    pattern B00,
    pattern B01,
    pattern B10,
    pattern B11,
 )

{- | Calendar format. See https://github.com/gritzko/ron/issues/19.
Year range is 2010—2350.
Precision is 100 ns.
-}
data CalendarTime = CalendarTime
    { months :: Word12
    , days :: Word6
    , hours :: Word6
    , minutes :: Word6
    , seconds :: Word6
    , nanosecHundreds :: Word24
    }
    deriving (Eq, Ord, Show)

newtype TimeVariety = TimeVariety Word2

pattern Calendar :: TimeVariety
pattern Calendar = TimeVariety B00

pattern Logical :: TimeVariety
pattern Logical = TimeVariety B01

{- | RFC 4122 epoch, hundreds of nanoseconds since 1582.
Year range is 1582—5235.
-}
pattern Epoch :: TimeVariety
pattern Epoch = TimeVariety B10

pattern Unknown :: TimeVariety
pattern Unknown = TimeVariety B11

{-# COMPLETE Calendar, Logical, Epoch, Unknown #-}

instance Show TimeVariety where
    show = \case
        Calendar {--} -> "Calendar"
        Logical {- -} -> "Logical"
        Epoch {-   -} -> "Epoch"
        Unknown {- -} -> "Unknown"

-- | Clock type is encoded in 2 higher bits of variety, value in uuidValue
newtype Time = Time Word64
    deriving (Eq, Ord)

instance Show Time where
    show t =
        show (timeVariety t) ++ '/' : BSC.unpack (encode60short $ timeValue t)

timeVariety :: Time -> TimeVariety
timeVariety (Time w64) = TimeVariety $ leastSignificant2 $ w64 `shiftR` 62

timeValue :: Time -> Word60
timeValue (Time w64) = ls60 w64

-- | Replica id assignment style
newtype OriginVariety = OriginVariety Word2
    deriving newtype (Eq, Hashable)

pattern TrieForked :: OriginVariety
pattern TrieForked = OriginVariety B00

pattern CryptoForked :: OriginVariety
pattern CryptoForked = OriginVariety B01

pattern RecordForked :: OriginVariety
pattern RecordForked = OriginVariety B10

pattern ApplicationSpecific :: OriginVariety
pattern ApplicationSpecific = OriginVariety B11

{-# COMPLETE TrieForked, CryptoForked, RecordForked, ApplicationSpecific #-}

instance Show OriginVariety where
    show = \case
        TrieForked -> "Trie"
        CryptoForked -> "Crypto"
        RecordForked -> "Record"
        ApplicationSpecific -> "App"

{- | Replica identifier.
Implementation: naming (62-61) and origin (60-0 bits) fields from UUID
-}
newtype Replica = Replica Word64
    deriving newtype (Eq, Hashable, Ord)

instance Show Replica where
    show (Replica w64) =
        show (OriginVariety $ leastSignificant2 $ w64 `shiftR` 60)
            ++ '/'
            : BSC.unpack (encode60short $ ls60 w64)

{- | Generic Lamport time event.
Cannot be 'Ord' because we can't compare different types of clocks.
If you want comparable events, use specific 'EpochEvent'.
-}
data Event = Event
    { time :: !Time
    , replica :: !Replica
    }
    deriving (Eq, Generic, Show)

class (Monad m) => ReplicaClock m where
    -- | Get current replica id
    getPid :: m Replica

    -- | Get sequential timestamps.
    --
    -- Laws:
    --
    -- 1. @
    -- t <- getEvents n
    -- (t !! i) == head t + i
    -- @
    --
    -- 2. @
    -- t1 <- 'getEvent'
    -- t2 <- 'getEvent'
    -- t2 >= t1 + 1
    -- @
    --
    -- 3. @getEvents 0 == getEvents 1@
    getEvents ::
        -- | number of needed timestamps
        Word60 ->
        m [Event]

    -- | Make local time not less than this
    advance :: Word60 -> m ()

instance (ReplicaClock m) => ReplicaClock (ExceptT e m) where
    getPid = lift getPid
    getEvents = lift . getEvents
    advance = lift . advance

instance (ReplicaClock m) => ReplicaClock (ReaderT r m) where
    getPid = lift getPid
    getEvents = lift . getEvents
    advance = lift . advance

instance (ReplicaClock m) => ReplicaClock (StateT s m) where
    getPid = lift getPid
    getEvents = lift . getEvents
    advance = lift . advance

instance (Monoid s, ReplicaClock m) => ReplicaClock (WriterT s m) where
    getPid = lift getPid
    getEvents = lift . getEvents
    advance = lift . advance

-- | 'advance' variant for any UUID
advanceToUuid :: (ReplicaClock clock) => UUID -> clock ()
advanceToUuid uuid =
    when (uuidVariant == B00 && uuidVersion == B10) $ advance uuidValue
  where
    UuidFields{uuidValue, uuidVariant, uuidVersion} = UUID.split uuid

-- | Get a single event
getEvent :: (HasCallStack, ReplicaClock m) => m Event
getEvent =
    getEvents 1 >>= \case
        e : _ -> pure e
        [] -> error "getEvents returned no events"

-- | Get a single event as UUID
getEventUuid :: (ReplicaClock m) => m UUID
getEventUuid = encodeEvent <$> getEvent

-- | Get event sequence as UUIDs
getEventUuids :: (ReplicaClock m) => Word60 -> m [UUID]
getEventUuids = fmap (map encodeEvent) . getEvents

encodeCalendar :: CalendarTime -> Word60
encodeCalendar CalendarTime{..} =
    ls60 $
        (safeCast months `shiftL` 48)
            .|. (safeCast days `shiftL` 42)
            .|. (safeCast hours `shiftL` 36)
            .|. (safeCast minutes `shiftL` 30)
            .|. (safeCast seconds `shiftL` 24)
            .|. safeCast nanosecHundreds

decodeCalendar :: Word60 -> CalendarTime
decodeCalendar w =
    CalendarTime
        { months = leastSignificant12 $ v `shiftR` 48
        , days = leastSignificant6 $ v `shiftR` 42
        , hours = leastSignificant6 $ v `shiftR` 36
        , minutes = leastSignificant6 $ v `shiftR` 30
        , seconds = leastSignificant6 $ v `shiftR` 24
        , nanosecHundreds = leastSignificant24 v
        }
  where
    v = safeCast w :: Word64

decodeTime :: Word64 -> Time
decodeTime value = Time $ value .&. 0x_CFFF_FFFF_FFFF_FFFF

encodeEvent :: Event -> UUID
encodeEvent Event{time, replica} =
    UUID (varietyAndValue .|. originVariety) (eventVersion .|. origin)
  where
    Time varietyAndValue = time
    (originVariety, origin) = encodeReplicaId replica
    eventVersion = 0x_2000_0000_0000_0000

decodeEvent :: UUID -> Event
decodeEvent u@(UUID x _) = Event{replica = decodeReplica u, time = decodeTime x}

decodeReplica :: UUID -> Replica
decodeReplica (UUID x y) =
    Replica $ (x .&. 0x_3000_0000_0000_0000) .|. (y .&. 0x_0FFF_FFFF_FFFF_FFFF)

encodeReplicaId :: Replica -> (Word64, Word64)
encodeReplicaId (Replica r) =
    ( r .&. 0x_3000_0000_0000_0000
    , r .&. 0x_0FFF_FFFF_FFFF_FFFF
    )

-- | Make a calendar timestamp from a date
mkCalendarDate ::
    -- | date as (year, month [1..12], day [1..])
    (Word16, Word16, Word8) ->
    Maybe CalendarTime
mkCalendarDate ymd = mkCalendarDateTime ymd (0, 0, 0)

-- | Make a calendar timestamp from a date and a day time
mkCalendarDateTime ::
    -- | date as (year, month [1..12], day [1..])
    (Word16, Word16, Word8) ->
    -- | day time as (hours, minutes, seconds)
    (Word8, Word8, Word8) ->
    Maybe CalendarTime
mkCalendarDateTime ymd hms = mkCalendarDateTimeNano ymd hms 0

-- | Make a calendar timestamp from a date, a day time, and a second fraction
mkCalendarDateTimeNano ::
    -- | date as (year, month [1..12], day [1..])
    (Word16, Word16, Word8) ->
    -- | day time as (hours, minutes, seconds)
    (Word8, Word8, Word8) ->
    -- | fraction of a second in hundreds of nanosecond
    Word32 ->
    Maybe CalendarTime
mkCalendarDateTimeNano (y, m, d) (hh, mm, ss) hns = do
    guard $ y >= 2010
    let months = (y - 2010) * 12 + m - 1
    guard $ months < 4096
    _ <- fromGregorianValid (fromIntegral y) (fromIntegral m) (fromIntegral d)
    _ <-
        makeTimeOfDayValid (fromIntegral hh) (fromIntegral mm) (fromIntegral ss)
    guard $ hns < 10_000_000
    pure
        CalendarTime
            { months = ls12 months
            , days = ls6 $ d - 1
            , hours = ls6 hh
            , minutes = ls6 mm
            , seconds = ls6 ss
            , nanosecHundreds = ls24 hns
            }

-- | Make a replica id from 'OriginVariety' and arbitrary number
mkReplica :: OriginVariety -> Word60 -> Replica
mkReplica (OriginVariety variety) origin =
    Replica $ (safeCast variety `shiftL` 60) .|. safeCast origin

mkTime :: TimeVariety -> Word60 -> Time
mkTime (TimeVariety variety) value =
    Time $ (safeCast variety `shiftL` 62) .|. safeCast value

mkCalendarEvent :: CalendarTime -> Replica -> Event
mkCalendarEvent time replica =
    Event{time = mkTime Calendar $ encodeCalendar time, replica}
