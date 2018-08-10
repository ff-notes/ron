{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Event
    ( ClockType (..)
    , Event (..)
    , Naming (..)
    , ReplicaId (..)
    , decodeEvent
    , encodeEvent
    ) where

import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Fixed (Fixed (MkFixed), Pico, resolution)
import           Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime),
                            fromGregorianValid, makeTimeOfDayValid,
                            timeOfDayToTime, timeToTimeOfDay, toGregorian,
                            utctDay, utctDayTime)

import           RON.Internal.Word (Word4, Word60, Word64, b00, b10,
                                    leastSignificant4, safeCast, toWord60)
import           RON.UUID (UUID, UuidFields (UuidFields), uuidOrigin,
                           uuidScheme, uuidValue, uuidVariant, uuidVariety)
import qualified RON.UUID as UUID

data ClockType
    = Base64Calendar
    | Logical
    | Epoch
    | Unknown
    deriving (Bounded, Enum, Eq, Ord, Show)

data Naming
    = TrieForked
    | CryptoForked
    | RecordForked
    | ApplicationSpecific
    deriving (Bounded, Enum, Eq, Ord, Show)

data ReplicaId = ReplicaId !ClockType !Naming !Word60
    deriving (Eq, Ord, Show)

-- | Lamport time
data Event = Event !UTCTime !ReplicaId
    deriving (Eq, Ord, Show)

encodeTime :: UTCTime -> Maybe Word60
encodeTime UTCTime{utctDay, utctDayTime} = do
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

decodeLocalTime :: Word60 -> Maybe UTCTime
decodeLocalTime w = do
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

encodeEvent :: Event -> Maybe UUID
encodeEvent (Event time replicaId) = do
    uuidValue <- encodeTime time
    let (uuidVariety, uuidOrigin) = encodeReplicaId replicaId
    pure $ UUID.build UuidFields
        { uuidVariety
        , uuidValue
        , uuidVariant = b00
        , uuidScheme  = b10
        , uuidOrigin
        }

decodeEvent :: UUID -> Maybe Event
decodeEvent uuid = Event
    <$> decodeLocalTime uuidValue
    <*> pure (decodeReplicaId uuidVariety uuidOrigin)
  where
    UuidFields{uuidVariety, uuidValue, uuidOrigin} = UUID.split uuid

decodeReplicaId :: Word4 -> Word60 -> ReplicaId
decodeReplicaId variety = ReplicaId clock naming
  where
    variety' = safeCast variety
    naming = toEnum $ variety' .&. 0b11
    clock  = toEnum $ (variety' `shiftR` 2) .&. 0b11

encodeReplicaId :: ReplicaId -> (Word4, Word60)
encodeReplicaId (ReplicaId clock naming origin) =
    ( leastSignificant4 $ (fromEnum clock `shiftL` 2) .|. fromEnum naming
    , origin
    )
