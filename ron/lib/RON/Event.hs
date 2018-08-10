{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Event
    ( CalendarEvent (..)
    , Clock (..)
    , EpochEvent (..)
    , Event (..)
    , LocalTime (..)
    , Naming (..)
    , Replica (..)
    , ReplicaId (..)
    , decodeEvent
    , encodeEvent
    , fromCalendarEvent
    , fromEpochEvent
    , getEvent
    , getEventUuid
    , getEventUuids
    ) where

import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Fixed (Fixed (MkFixed), Pico, resolution)
import           Data.Hashable (Hashable, hashUsing, hashWithSalt)
import           Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime),
                            fromGregorianValid, makeTimeOfDayValid,
                            timeOfDayToTime, timeToTimeOfDay, toGregorian,
                            utctDay, utctDayTime)
import           Data.Traversable (for)

import           RON.Internal.Word (pattern B00, pattern B01, pattern B10,
                                    pattern B11, Word2, Word60, Word64, Word8,
                                    leastSignificant2, leastSignificant4,
                                    leastSignificant60, safeCast, toWord60)
import           RON.UUID (UUID, UuidFields (UuidFields), uuidOrigin,
                           uuidScheme, uuidValue, uuidVariant, uuidVariety)
import qualified RON.UUID as UUID

-- | Clock type is encoded in 2 higher bits of variety, value in uuidValue
data LocalTime
    = Calendar !UTCTime
        -- ^ Calendar format. https://github.com/gritzko/ron/issues/19.
        -- Year range is 2010—2350.
        -- Precision is 100 ns.
    | Logical !Word60
        -- ^ https://en.wikipedia.org/wiki/Logical_clock
    | Epoch !Word60
        -- ^ RFC 4122 epoch, hundreds of nanoseconds since 1582.
        -- Year range is 1582—5234.
    | Unknown !Word60
    deriving (Eq, Show)

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
data CalendarEvent = CalendarEvent !UTCTime !ReplicaId
    deriving (Eq, Show)

instance Ord CalendarEvent where
    compare (CalendarEvent t1 (ReplicaId n1 r1))
            (CalendarEvent t2 (ReplicaId n2 r2))
        = compare
            (t1, fromEnum n1, r1)
            (t2, fromEnum n2, r2)

fromCalendarEvent :: CalendarEvent -> Event
fromCalendarEvent (CalendarEvent t r) = Event (Calendar t) r

-- | Epoch-based Lamport time event, specific case of 'Event'.
data EpochEvent = EpochEvent !Word60 !ReplicaId
    deriving (Eq, Show)

instance Ord EpochEvent where
    compare (EpochEvent t1 (ReplicaId n1 r1))
            (EpochEvent t2 (ReplicaId n2 r2))
        = compare
            (t1, fromEnum n1, r1)
            (t2, fromEnum n2, r2)

fromEpochEvent :: EpochEvent -> Event
fromEpochEvent (EpochEvent t r) = Event (Epoch t) r

class Monad m => Replica m where
    getPid :: m ReplicaId

class Replica m => Clock m where
    -- | Get sequential timestamps.
    --
    -- Laws:
    --    1.  t1 <- getEvents n
    --        t2 <- getEvent
    --        t2 >= t1 + n
    --
    --    2. getEvents 0 == getEvents 1
    getEvents
        :: Word60 -- ^ number of needed timestamps
        -> m [EpochEvent]
        -- ^ Starting value of the range.
        -- So return value @t@ means range @[t .. t + n - 1]@.

    -- | Make local time not less than this
    advance :: Word60 -> m ()

getEvent :: Clock m => m EpochEvent
getEvent = head <$> getEvents (leastSignificant60 (1 :: Word64))

getEventUuid :: Clock m => m UUID
getEventUuid = do
    e <- getEvent
    maybe (fail $ "bad event: " ++ show e) pure $ encodeEvent $ fromEpochEvent e

getEventUuids :: Clock m => Word60 -> m [UUID]
getEventUuids n = do
    es <- getEvents n
    for es $ \e ->
        maybe (fail $ "bad event: " ++ show e) pure $
        encodeEvent $ fromEpochEvent e

encodeUTCTime :: UTCTime -> Maybe Word60
encodeUTCTime UTCTime{utctDay, utctDayTime} = do
    yearRel2010 <-
        if year >= 2010 then pure $ fromIntegral year - 2010 else Nothing
    let months = yearRel2010 * 12 + (month - 1)
    secs <- if rawSecs < 60 then pure $ fromIntegral rawSecs else Nothing
    toWord60 $
        (months     `shiftL` 48) .|.
        ((days - 1) `shiftL` 42) .|.
        (hours      `shiftL` 36) .|.
        (mins       `shiftL` 30) .|.
        (secs       `shiftL` 24) .|.
        ns100
  where
    ns100 = fromIntegral $ ps `div` (100 * 1000)  -- <= 10^7
    (rawSecs, ps) = s `divMod` picoResolution
    MkFixed s = fs :: Pico
    picoResolution = resolution fs  -- 10^12
    TimeOfDay (fromIntegral -> hours) (fromIntegral -> mins) fs =
        timeToTimeOfDay utctDayTime
    (year, fromIntegral -> month, fromIntegral -> days) =
        toGregorian utctDay

decodeUTCTime :: Word60 -> Maybe UTCTime
decodeUTCTime w = do
    utctDay <- fromGregorianValid (year + 2010) (month + 1) (days + 1)
    timeOfDay <- makeTimeOfDayValid
        hours mins (secs + (MkFixed $ ns100 * 100 * 1000 :: Pico))
    pure UTCTime{utctDay, utctDayTime = timeOfDayToTime timeOfDay}
  where
    v = safeCast w :: Word64
    ns100  = safeCast     $  v              .&. 0xFFFFFF
    secs   = safeCast     $ (v `shiftR` 24) .&. 0x3F
    mins   = fromIntegral $ (v `shiftR` 30) .&. 0x3F
    hours  = fromIntegral $ (v `shiftR` 36) .&. 0x3F
    days   = fromIntegral $ (v `shiftR` 42) .&. 0x3F
    months =                (v `shiftR` 48) .&. 0xFFF
    (safeCast -> year, fromIntegral -> month) = months `divMod` 12

encodeLocalTime :: LocalTime -> Maybe (Word2, Word60)
encodeLocalTime = \case
    Calendar t -> (B00,) <$> encodeUTCTime t
    Logical  t -> pure (B01, t)
    Epoch    t -> pure (B10, t)
    Unknown  t -> pure (B11, t)

decodeLocalTime :: Word2 -> Word60 -> Maybe LocalTime
decodeLocalTime clockTypeCode value = case clockTypeCode of
    B00 -> Calendar <$> decodeUTCTime value
    B01 -> pure $ Logical value
    B10 -> pure $ Epoch   value
    B11 -> pure $ Unknown value

encodeEvent :: Event -> Maybe UUID
encodeEvent (Event time replicaId) = do
    (varietyMS2, uuidValue) <- encodeLocalTime time
    let (varietyLS2, uuidOrigin) = encodeReplicaId replicaId
        uuidVariety =
            leastSignificant4 $
                ((safeCast varietyMS2 :: Word8) `shiftL` 2) .|.
                ( safeCast varietyLS2 :: Word8)
    pure $ UUID.build UuidFields
        { uuidVariety
        , uuidValue
        , uuidVariant = B00
        , uuidScheme  = B10
        , uuidOrigin
        }

decodeEvent :: UUID -> Maybe Event
decodeEvent uuid = Event
    <$> decodeLocalTime
            (leastSignificant2 (safeCast uuidVariety `shiftR` 2 :: Word8))
            uuidValue
    <*> pure
            (decodeReplicaId
                (leastSignificant2 (safeCast uuidVariety :: Word8))
                uuidOrigin)
  where
    UuidFields{uuidVariety, uuidValue, uuidOrigin} = UUID.split uuid

decodeReplicaId :: Word2 -> Word60 -> ReplicaId
decodeReplicaId varietyLS2 = ReplicaId $ toEnum $ safeCast varietyLS2

encodeReplicaId :: ReplicaId -> (Word2, Word60)
encodeReplicaId (ReplicaId naming origin) =
    ( leastSignificant2 $ fromEnum naming
    , origin
    )
