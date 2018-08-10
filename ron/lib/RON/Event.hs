{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Event
    ( getCalendarEvent
    , mkCalendarEvent
    ) where

import           RON.Internal.Prelude

import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Fixed (Fixed (MkFixed), Pico, resolution)
import           Data.Time (TimeOfDay (..), UTCTime (..), fromGregorianValid,
                            makeTimeOfDayValid, timeOfDayToTime,
                            timeToTimeOfDay, toGregorian)

mkCalendarEvent :: UTCTime -> Maybe Word60
mkCalendarEvent UTCTime{utctDay, utctDayTime} = do
    guard $ year >= 2010
    guard $ secs < 60
    pure $
        (months     `shiftL` 48) .|.
        ((days - 1) `shiftL` 42) .|.
        (hours      `shiftL` 36) .|.
        (mins       `shiftL` 30) .|.
        (secs       `shiftL` 24) .|.
        ns100
  where
    ns100 = ps `div` (100 * 1000)
    (fromIntegral -> secs, fromIntegral -> ps) = s `divMod` resolution fs
    MkFixed s = fs :: Pico
    TimeOfDay (fromIntegral -> hours) (fromIntegral -> mins) fs =
        timeToTimeOfDay utctDayTime
    months = (fromIntegral year - 2010) * 12 + (month - 1)
    (year, fromIntegral -> month, fromIntegral -> days) =
        toGregorian utctDay

getCalendarEvent :: Word60 -> Maybe UTCTime
getCalendarEvent v = do
    utctDay <- fromGregorianValid (year + 2010) (month + 1) (days + 1)
    timeOfDay <- makeTimeOfDayValid
        hours mins (secs + (MkFixed $ ns100 * 100 * 1000 :: Pico))
    pure UTCTime{utctDay, utctDayTime = timeOfDayToTime timeOfDay}
  where
    ns100  = fromIntegral $  v              .&. 0xFFFFFF
    secs   = fromIntegral $ (v `shiftR` 24) .&. 0x3F
    mins   = fromIntegral $ (v `shiftR` 30) .&. 0x3F
    hours  = fromIntegral $ (v `shiftR` 36) .&. 0x3F
    days   = fromIntegral $ (v `shiftR` 42) .&. 0x3F
    months =                (v `shiftR` 48) .&. 0xFFF
    (fromIntegral -> year, fromIntegral -> month) = months `divMod` 12

-- TODO
-- decodeCalendarEvent :: ByteString -> UTCTime
-- decodeCalendarEvent = _

-- TODO
-- encodeCalendarEvent :: UTCTime -> ByteString
-- encodeCalendarEvent = _
