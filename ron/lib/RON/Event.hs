{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Event
    ( CalendarTime (..)
    , CalendarEvent (..)
    , EpochEvent (..)
    , EpochTime
    , Event (..)
    , LocalTime (..)
    , Naming (..)
    , ReplicaClock (..)
    , ReplicaId (..)
    , advanceToUuid
    , applicationSpecific
    , decodeEvent
    , encodeEvent
    , fromCalendarEvent
    , fromEpochEvent
    , getEvent
    , getEventUuid
    , getEventUuids
    , mkCalendarDate
    , mkCalendarDateTime
    , mkCalendarDateTimeNano
    , toEpochEvent
    ) where

import           Control.Monad.Except (ExceptT, lift)
import           Control.Monad.State.Strict (StateT)
import           Data.Bits (shiftL, shiftR, (.|.))
import           Data.Hashable (Hashable, hashUsing, hashWithSalt)

import           RON.Internal.Word (pattern B00, pattern B01, pattern B10,
                                    pattern B11, Word12, Word16, Word2, Word24,
                                    Word32, Word6, Word60, Word64, Word8,
                                    leastSignificant12, leastSignificant2,
                                    leastSignificant24, leastSignificant4,
                                    leastSignificant6, ls12, ls24, ls6, ls60,
                                    safeCast)
import           RON.UUID (UUID, UuidFields (UuidFields), uuidOrigin,
                           uuidScheme, uuidValue, uuidVariant, uuidVariety)
import qualified RON.UUID as UUID

-- | Calendar format. See https://github.com/gritzko/ron/issues/19.
-- Year range is 2010—2350.
-- Precision is 100 ns.
data CalendarTime = CalendarTime
    { months          :: Word12
    , days            :: Word6
    , hours           :: Word6
    , minutes         :: Word6
    , seconds         :: Word6
    , nanosecHundreds :: Word24
    }
    deriving (Eq, Ord, Show)

-- | RFC 4122 epoch, hundreds of nanoseconds since 1582.
-- Year range is 1582—5235.
type EpochTime = Word60

-- | Clock type is encoded in 2 higher bits of variety, value in uuidValue
data LocalTime
    = TCalendar !CalendarTime
    | TLogical !Word60
        -- ^ https://en.wikipedia.org/wiki/Logical_clock
    | TEpoch !EpochTime
    | TUnknown !Word60
    deriving (Eq, Show)

-- | Replica id assignment style
data Naming
    = TrieForked
    | CryptoForked
    | RecordForked
    | ApplicationSpecific
    deriving (Bounded, Enum, Eq, Show)

instance Hashable Naming where
    hashWithSalt = hashUsing fromEnum

data ReplicaId = ReplicaId !Naming !Word60
    deriving (Eq, Show)

instance Hashable ReplicaId where
    hashWithSalt = hashUsing $ \(ReplicaId n r) -> (n, r)

-- | Generic Lamport time event.
-- Cannot be 'Ord' because we can't compare different types of clocks.
-- If you want comparable events, use specific 'EpochEvent'.
data Event = Event !LocalTime !ReplicaId
    deriving (Eq, Show)

-- | Calendar-based Lamport time event, specific case of 'Event'.
data CalendarEvent = CalendarEvent !CalendarTime !ReplicaId
    deriving (Eq, Show)

instance Ord CalendarEvent where
    compare (CalendarEvent t1 (ReplicaId n1 r1))
            (CalendarEvent t2 (ReplicaId n2 r2))
        = compare
            (t1, fromEnum n1, r1)
            (t2, fromEnum n2, r2)

fromCalendarEvent :: CalendarEvent -> Event
fromCalendarEvent (CalendarEvent t r) = Event (TCalendar t) r

-- | Epoch-based Lamport time event, specific case of 'Event'.
data EpochEvent = EpochEvent !EpochTime !ReplicaId
    deriving (Eq, Show)

instance Ord EpochEvent where
    compare (EpochEvent t1 (ReplicaId n1 r1))
            (EpochEvent t2 (ReplicaId n2 r2))
        = compare
            (t1, fromEnum n1, r1)
            (t2, fromEnum n2, r2)

fromEpochEvent :: EpochEvent -> Event
fromEpochEvent (EpochEvent t r) = Event (TEpoch t) r

toEpochEvent :: Event -> Maybe EpochEvent
toEpochEvent (Event t r) = case t of
    TEpoch t' -> Just $ EpochEvent t' r
    _         -> Nothing

class Monad m => ReplicaClock m where

    -- | Get current replica id
    getPid :: m ReplicaId

    -- | Get sequential timestamps.
    --
    -- Laws:
    --
    -- 1. @
    --t1 <- getEvents n
    --t2 <- getEvent
    --t2 >= t1 + n
    -- @
    --
    -- 2. @getEvents 0 == getEvents 1@
    getEvents
        :: EpochTime -- ^ number of needed timestamps
        -> m [EpochEvent]
        -- ^ Starting value of the range.
        -- So return value @t@ means range @[t .. t + n - 1]@.

    -- | Make local time not less than this
    advance :: EpochTime -> m ()

instance ReplicaClock m => ReplicaClock (ExceptT e m) where
    getPid    = lift   getPid
    getEvents = lift . getEvents
    advance   = lift . advance

instance ReplicaClock m => ReplicaClock (StateT s m) where
    getPid    = lift   getPid
    getEvents = lift . getEvents
    advance   = lift . advance

advanceToUuid :: ReplicaClock clock => UUID -> clock ()
advanceToUuid = advance . uuidValue . UUID.split

getEvent :: ReplicaClock m => m EpochEvent
getEvent = head <$> getEvents (ls60 1)

getEventUuid :: ReplicaClock m => m UUID
getEventUuid = encodeEvent . fromEpochEvent <$> getEvent

getEventUuids :: ReplicaClock m => Word60 -> m [UUID]
getEventUuids = fmap (map $ encodeEvent . fromEpochEvent) . getEvents

encodeCalendar :: CalendarTime -> Word60
encodeCalendar CalendarTime{..} = ls60 $
    (safeCast months     `shiftL` 48) .|.
    (safeCast days       `shiftL` 42) .|.
    (safeCast hours      `shiftL` 36) .|.
    (safeCast minutes    `shiftL` 30) .|.
    (safeCast seconds    `shiftL` 24) .|.
    safeCast  nanosecHundreds

decodeCalendar :: Word60 -> CalendarTime
decodeCalendar w = CalendarTime
    { months          = leastSignificant12 $ v `shiftR` 48
    , days            = leastSignificant6  $ v `shiftR` 42
    , hours           = leastSignificant6  $ v `shiftR` 36
    , minutes         = leastSignificant6  $ v `shiftR` 30
    , seconds         = leastSignificant6  $ v `shiftR` 24
    , nanosecHundreds = leastSignificant24   v
    }
  where
    v = safeCast w :: Word64

encodeLocalTime :: LocalTime -> (Word2, Word60)
encodeLocalTime = \case
    TCalendar t -> (B00, encodeCalendar t)
    TLogical  t -> (B01, t)
    TEpoch    t -> (B10, t)
    TUnknown  t -> (B11, t)

decodeLocalTime :: Word2 -> Word60 -> LocalTime
decodeLocalTime = \case
    B00 -> TCalendar . decodeCalendar
    B01 -> TLogical
    B10 -> TEpoch
    B11 -> TUnknown

encodeEvent :: Event -> UUID
encodeEvent (Event time replicaId) = UUID.build UuidFields
    { uuidVariety
    , uuidValue
    , uuidVariant = B00
    , uuidScheme  = B10
    , uuidOrigin
    }
  where
    (varietyMS2, uuidValue) = encodeLocalTime time
    (varietyLS2, uuidOrigin) = encodeReplicaId replicaId
    uuidVariety = leastSignificant4 $
        ((safeCast varietyMS2 :: Word8) `shiftL` 2) .|.
        ( safeCast varietyLS2 :: Word8)

decodeEvent :: UUID -> Event
decodeEvent uuid = Event
    (decodeLocalTime
        (leastSignificant2 (safeCast uuidVariety `shiftR` 2 :: Word8))
        uuidValue)
    (decodeReplicaId
        (leastSignificant2 (safeCast uuidVariety :: Word8)) uuidOrigin)
  where
    UuidFields{uuidVariety, uuidValue, uuidOrigin} = UUID.split uuid

decodeReplicaId :: Word2 -> Word60 -> ReplicaId
decodeReplicaId varietyLS2 = ReplicaId $ toEnum $ safeCast varietyLS2

encodeReplicaId :: ReplicaId -> (Word2, Word60)
encodeReplicaId (ReplicaId naming origin) =
    ( leastSignificant2 $ fromEnum naming
    , origin
    )

mkCalendarDate :: (Word16, Word16, Word8) -> Maybe CalendarTime
mkCalendarDate ymd = mkCalendarDateTime ymd (0, 0, 0)

mkCalendarDateTime
    :: (Word16, Word16, Word8) -> (Word8, Word8, Word8) -> Maybe CalendarTime
mkCalendarDateTime ymd hms = mkCalendarDateTimeNano ymd hms 0

mkCalendarDateTimeNano
    :: (Word16, Word16, Word8)
    -> (Word8,  Word8,  Word8)
    -> Word32
    -> Maybe CalendarTime
mkCalendarDateTimeNano (y, m, d) (hh, mm, ss) ns =
    -- TODO(2018-08-19, cblp) check bounds
    pure CalendarTime
        { months          = ls12 $ (y - 2010) * 12 + m - 1
        , days            = ls6  $ d - 1
        , hours           = ls6  hh
        , minutes         = ls6  mm
        , seconds         = ls6  ss
        , nanosecHundreds = ls24 ns
        }

applicationSpecific :: Word64 -> ReplicaId
applicationSpecific = ReplicaId ApplicationSpecific . ls60
