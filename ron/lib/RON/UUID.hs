{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , succValue
    , zero
    , pattern Zero
    -- * Name
    , getName
    , liftName
    , mkName
    , mkScopedName
    -- * Base32 encoding, suitable for file names
    , decodeBase32
    , encodeBase32
    ) where

import           RON.Internal.Prelude

import           Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString.Char8 as BSC
import           Language.Haskell.TH.Syntax (Exp, Q, liftData)

import qualified RON.Base64 as Base64
import           RON.Util.Word (pattern B00, pattern B0000, pattern B01,
                                pattern B10, pattern B11, Word2, Word4, Word60,
                                leastSignificant2, leastSignificant4,
                                leastSignificant60, safeCast)

-- | Universally unique identifier of anything
data UUID = UUID
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Data, Eq, Generic, Hashable, Ord)

-- | RON-Text-encoding
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
        y' = case (uuidVersion, uuidOrigin) of
            (B00, safeCast -> 0 :: Word64) -> ""
            _ -> version : BSC.unpack (Base64.encode60short uuidOrigin)
        generic = BSC.unpack $ Base64.encode64 x <> Base64.encode64 y
        version = case uuidVersion of
            B00 -> '$'
            B01 -> '%'
            B10 -> '+'
            B11 -> '-'

-- | UUID split in parts
data UuidFields = UuidFields
    { uuidVariety :: !Word4
    , uuidValue   :: !Word60
    , uuidVariant :: !Word2
    , uuidVersion :: !Word2
    , uuidOrigin  :: !Word60
    }
    deriving (Eq, Show)

-- | Split UUID into parts
split :: UUID -> UuidFields
split (UUID x y) = UuidFields
    { uuidVariety = leastSignificant4 $ x `shiftR` 60
    , uuidValue   = leastSignificant60  x
    , uuidVariant = leastSignificant2 $ y `shiftR` 62
    , uuidVersion = leastSignificant2 $ y `shiftR` 60
    , uuidOrigin  = leastSignificant60  y
    }

-- | Build UUID from parts
build :: UuidFields -> UUID
build UuidFields{..} = UUID
    (buildX uuidVariety uuidValue)
    (buildY uuidVariant uuidVersion uuidOrigin)

-- | Build former 64 bits of UUID from parts
buildX :: Word4 -> Word60 -> Word64
buildX uuidVariety uuidValue =
    (safeCast uuidVariety `shiftL` 60) .|. safeCast uuidValue

-- | Build latter 64 bits of UUID from parts
buildY :: Word2 -> Word2 -> Word60 -> Word64
buildY uuidVariant uuidVersion uuidOrigin
    =   (safeCast uuidVariant `shiftL` 62)
    .|. (safeCast uuidVersion `shiftL` 60)
    .|.  safeCast uuidOrigin

-- | Make an unscoped (unqualified) name
mkName
    :: Monad m
    => ByteString  -- ^ name, max 10 Base64 letters
    -> m UUID
mkName nam = mkScopedName nam ""

-- | Contruct a UUID name in compile-time
liftName :: ByteString -> Q Exp
liftName = mkName >=> liftData
-- TODO(2019-01-11, cblp) typed splice

-- | Make a scoped (qualified) name
mkScopedName
    :: Monad m
    => ByteString  -- ^ scope, max 10 Base64 letters
    -> ByteString  -- ^ local name, max 10 Base64 letters
    -> m UUID
mkScopedName scope nam = do
    scope' <- maybe (fail "Bad scope") pure $ Base64.decode60 scope
    nam'   <- maybe (fail "Bad name")  pure $ Base64.decode60 nam
    pure $ build UuidFields
        { uuidVariety = B0000
        , uuidValue   = scope'
        , uuidVariant = B00
        , uuidVersion = B00
        , uuidOrigin  = nam'
        }

-- | Convert UUID to a name
getName
    :: UUID
    -> Maybe (ByteString, ByteString)
        -- ^ @(scope, name)@ for a scoped name; @(name, "")@ for a global name
getName uuid = case split uuid of
    UuidFields{uuidVariety = B0000, uuidVariant = B00, uuidVersion = B00, ..} ->
        Just (x, y)
      where
        x = Base64.encode60short uuidValue
        y = case safeCast uuidOrigin :: Word64 of
            0 -> ""
            _ -> Base64.encode60short uuidOrigin
    _ -> Nothing

-- | UUID with all zero fields
zero :: UUID
zero = UUID 0 0

-- | UUID with all zero fields
pattern Zero :: UUID
pattern Zero = UUID 0 0

-- | Increment field 'uuidValue' of a UUID
succValue :: UUID -> UUID
succValue = build . go . split where
    go u@UuidFields{uuidValue} = u
        {uuidValue = if uuidValue < maxBound then succ uuidValue else uuidValue}

-- | Encode a UUID to a Base32 string
encodeBase32 :: UUID -> FilePath
encodeBase32 (UUID x y) =
    BSC.unpack $
    Base64.encode64base32short x <> "-" <> Base64.encode64base32short y

-- | Decode a UUID from a Base32 string
decodeBase32 :: FilePath -> Maybe UUID
decodeBase32 fp = do
    let (x, dashy) = span (/= '-') $ map toUpper fp
    ("-", y) <- pure $ splitAt 1 dashy
    UUID
        <$> Base64.decode64base32 (BSC.pack x)
        <*> Base64.decode64base32 (BSC.pack y)
