{-# OPTIONS_GHC -ddump-deriv #-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.UUID
    ( UUID (..)
    , UuidFields (..)
    , mkName
    , mkNameUnsafe
    , mkScopedName
    , mkScopedNameUnsafe
    , split
    ) where

import           Internal.Prelude

import           Data.Bits (shiftR, (.&.))
import           Numeric (showHex)

import qualified RON.Base64 as Base64

-- | Universally unique identifier of anything
data UUID = UUID
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq)

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
