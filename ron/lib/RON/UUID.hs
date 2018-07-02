{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module RON.UUID
    ( UUID (..)
    , UuidFields (..)
    , getCalendarEvent
    , mkCalendarEvent
    , mkName
    , mkNameUnsafe
    , mkScopedName
    , mkScopedNameUnsafe
    , split
    ) where

import           Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Fixed (Fixed (MkFixed), Pico, resolution)
import           Data.Time (TimeOfDay (..), UTCTime (..), fromGregorianValid,
                            makeTimeOfDayValid, timeOfDayToTime,
                            timeToTimeOfDay, toGregorian)
import           GHC.Generics (Generic)
import           Numeric (showHex)

import qualified RON.Base64 as Base64

-- | Universally unique identifier of anything
data UUID = UUID
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq, Generic, NFData)

instance Show UUID where
    showsPrec a (UUID x y) =
        showParen (a >= 11) $
        showString "UUID 0x" . showHex x . showString " 0x" . showHex y

data UuidFields = UuidFields
    { variety :: Word4
    , value   :: Word60
    , variant :: Word2
    , scheme  :: Word2
    , origin  :: Word60
    }
    deriving Show

split :: UUID -> UuidFields
split (UUID x y) = UuidFields
    { variety = fromIntegral $     x `shiftR` 60
    , value   = leastSignificant60 x
    , variant = fromIntegral $     y `shiftR` 62
    , scheme  = fromIntegral $    (y `shiftR` 60) .&. 0b11
    , origin  = leastSignificant60 y
    }

-- | Make an unscoped (unqualified) name
mkName
    :: ByteString  -- ^ name, max 10 Base64 letters
    -> Maybe UUID
mkName nam = mkScopedName nam ""

-- | Partial version of 'mkName'.
mkNameUnsafe
    :: ByteString  -- ^ name, max 10 Base64 letters
    -> UUID
mkNameUnsafe = fromJust . mkName

-- | Make a scoped (qualified) name
mkScopedName
    :: ByteString  -- ^ scope, max 10 Base64 letters
    -> ByteString  -- ^ local name, max 10 Base64 letters
    -> Maybe UUID
mkScopedName scope nam = do
    scope' <- Base64.decode60 scope
    nam'   <- Base64.decode60 nam
    pure $ UUID scope' nam'

-- | Partial version of 'mkScopedName'.
mkScopedNameUnsafe
    :: ByteString  -- ^ scope, max 10 Base64 letters
    -> ByteString  -- ^ local name, max 10 Base64 letters
    -> UUID
mkScopedNameUnsafe scope nam = fromJust $ mkScopedName scope nam

-- | Year must be >= 2010.
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

-- TODO
-- mkCalendarEventUuid :: UTCTime -> Maybe Word60 -> UUID
-- mkCalendarEventUuid = _

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
