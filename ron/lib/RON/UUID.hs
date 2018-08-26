{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RON.UUID
    ( UUID (..)
    , UuidFields (..)
    , build
    , buildX
    , buildY
    , split
    , pattern Zero
    , zero
    -- * Name
    , getName
    , mkName
    , mkScopedName
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (chr)
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

import qualified RON.Base64 as Base64
import           RON.Internal.Word (pattern B00, pattern B0000, pattern B01,
                                    pattern B10, pattern B11, Word2, Word4,
                                    Word60, leastSignificant2,
                                    leastSignificant4, leastSignificant60,
                                    safeCast)

-- | Universally unique identifier of anything
data UUID = UUID
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq, Generic, Hashable, NFData, Ord)

instance Show UUID where
    -- showsPrec a (UUID x y) =
    --     showParen (a >= 11) $
    --     showString "UUID 0x" . showHex x . showString " 0x" . showHex y
    show this = show serialized
      where
        UUID x y = this
        UuidFields{..} = split this
        serialized = case uuidVariant of
            B00 -> unzipped
            _   -> generic
        unzipped = x' <> y'
        variety = case uuidVariety of
            B0000 -> ""
            _     -> chr (fromIntegral $ Base64.encodeLetter4 uuidVariety) : "/"
        x' = variety <> BSC.unpack (Base64.encode60short uuidValue)
        y' = case (uuidScheme, uuidOrigin) of
            (B00, safeCast -> 0 :: Word64) -> ""
            _ -> scheme : BSC.unpack (Base64.encode60short uuidOrigin)
        generic = BSC.unpack $ Base64.encode64 x <> Base64.encode64 y
        scheme = case uuidScheme of
            B00 -> '$'
            B01 -> '%'
            B10 -> '+'
            B11 -> '-'

data UuidFields = UuidFields
    { uuidVariety :: !Word4
    , uuidValue   :: !Word60
    , uuidVariant :: !Word2
    , uuidScheme  :: !Word2
    , uuidOrigin  :: !Word60
    }
    deriving (Eq, Show)

split :: UUID -> UuidFields
split (UUID x y) = UuidFields
    { uuidVariety = leastSignificant4 $ x `shiftR` 60
    , uuidValue   = leastSignificant60  x
    , uuidVariant = leastSignificant2 $ y `shiftR` 62
    , uuidScheme  = leastSignificant2 $ y `shiftR` 60
    , uuidOrigin  = leastSignificant60  y
    }

build :: UuidFields -> UUID
build UuidFields{..} = UUID
    (buildX uuidVariety uuidValue)
    (buildY uuidVariant uuidScheme uuidOrigin)

buildX :: Word4 -> Word60 -> Word64
buildX uuidVariety uuidValue =
    (safeCast uuidVariety `shiftL` 60) .|. safeCast uuidValue

buildY :: Word2 -> Word2 -> Word60 -> Word64
buildY uuidVariant uuidScheme uuidOrigin
    =   (safeCast uuidVariant `shiftL` 62)
    .|. (safeCast uuidScheme  `shiftL` 60)
    .|.  safeCast uuidOrigin

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
    pure $ build UuidFields
        { uuidVariety = B0000
        , uuidValue   = scope'
        , uuidVariant = B00
        , uuidScheme  = B00
        , uuidOrigin  = nam'
        }

getName
    :: UUID
    -> Maybe (ByteString, ByteString)
        -- ^ @(scope, name)@ for a scoped name; @(name, "")@ for a global name
getName uuid = case split uuid of
    UuidFields{uuidVariety = B0000, uuidVariant = B00, uuidScheme = B00, ..} ->
        Just (x, y)
      where
        x = Base64.encode60short uuidValue
        y = case safeCast uuidOrigin :: Word64 of
            0 -> ""
            _ -> Base64.encode60short uuidOrigin
    _ -> Nothing

zero :: UUID
zero = UUID 0 0

pattern Zero :: UUID
pattern Zero = UUID 0 0
