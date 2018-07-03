{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.UUID
    ( UUID (..)
    , UuidFields (..)
    , getCalendarEvent
    , mkCalendarEvent
    , mkName
    , mkScopedName
    , split
    ) where

import           Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Time (UTCTime (..))
import           GHC.Generics (Generic)
import           Numeric (showHex)

import qualified RON.Base64 as Base64
import qualified RON.Event as Event

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
    { uuidVariety :: Word4
    , uuidValue   :: Word60
    , uuidVariant :: Word2
    , uuidScheme  :: Word2
    , uuidOrigin  :: Word60
    }
    deriving Show

split :: UUID -> UuidFields
split (UUID x y) = UuidFields
    { uuidVariety = fromIntegral $     x `shiftR` 60
    , uuidValue   = leastSignificant60 x
    , uuidVariant = fromIntegral $     y `shiftR` 62
    , uuidScheme  = fromIntegral $    (y `shiftR` 60) .&. 0b11
    , uuidOrigin  = leastSignificant60 y
    }

build :: UuidFields -> UUID
build UuidFields{..} = UUID x y
  where
    x = (fromIntegral (uuidVariety .&. 0b1111) `shiftL` 60) .|.
        leastSignificant60 uuidValue
    y = (fromIntegral (uuidVariant .&.   0b11) `shiftL` 62) .|.
        (fromIntegral (uuidScheme  .&.   0b11) `shiftL` 60) .|.
        leastSignificant60 uuidOrigin

-- | Make an unscoped (unqualified) name
mkName
    :: ByteString  -- ^ name, max 10 Base64 letters
    -> Maybe UUID
mkName nam = mkScopedName nam ""

-- | Make a scoped (qualified) name
mkScopedName
    :: ByteString  -- ^ scope, max 10 Base64 letters
    -> ByteString  -- ^ local name, max 10 Base64 letters
    -> Maybe UUID
mkScopedName scope nam = do
    scope' <- Base64.decode60 scope
    nam'   <- Base64.decode60 nam
    pure $ UUID scope' nam'

mkCalendarEvent :: UTCTime -> (Word2, Word60) -> Maybe UUID
mkCalendarEvent time (replicaAssignmentRule, uuidOrigin) = do
    uuidValue <- Event.mkCalendarEvent time
    pure $ build UuidFields
        { uuidVariety = 0b0000 .|. (replicaAssignmentRule .&. 0b11)
        , uuidValue
        , uuidVariant = 0b00
        , uuidScheme  = 0b10
        , uuidOrigin
        }

getCalendarEvent :: UUID -> Maybe UTCTime
getCalendarEvent = Event.getCalendarEvent . uuidValue . split
