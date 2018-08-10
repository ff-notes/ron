{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RON.Internal.Word
    (
    -- * Word2
      Word2
    , b00, b01, b10, b11
    , pattern B00, pattern B01, pattern B10, pattern B11
    , leastSignificant2
    -- * Word4
    , Word4
    , b0000, b0001, b0010, b0011, b0100, b0101, b0110, b0111
    , b1000, b1001, b1010, b1011, b1100, b1101, b1110, b1111
    , pattern B0000
    , leastSignificant4
    -- * Word6
    , Word6
    , leastSignificant6
    -- * Word60
    , Word60
    , leastSignificant60
    , toWord60
    -- * Word64
    , Word64
    -- * SafeCast
    , SafeCast (..)
    ) where

import           Data.Bits ((.&.))
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed, HasResolution)
import           Data.Word (Word32, Word64, Word8)

newtype Word2 = W2 Word8
    deriving (Eq, Ord, Show)

b00, b01, b10, b11 :: Word2
b00 = W2 0b00
b01 = W2 0b01
b10 = W2 0b10
b11 = W2 0b11

pattern B00 :: Word2
pattern B00 = W2 0b00
pattern B01 :: Word2
pattern B01 = W2 0b01
pattern B10 :: Word2
pattern B10 = W2 0b10
pattern B11 :: Word2
pattern B11 = W2 0b11
{-# COMPLETE B00, B01, B10, B11 #-}

-- | 'Word2' smart constructor dropping upper bits
leastSignificant2 :: Integral integral => integral -> Word2
leastSignificant2 = W2 . (0b11 .&.) . fromIntegral

newtype Word4 = W4 Word8
    deriving (Eq, Ord, Show)

b0000, b0001, b0010, b0011, b0100, b0101, b0110, b0111 :: Word4
b1000, b1001, b1010, b1011, b1100, b1101, b1110, b1111 :: Word4
b0000 = W4 0b0000
b0001 = W4 0b0001
b0010 = W4 0b0010
b0011 = W4 0b0011
b0100 = W4 0b0100
b0101 = W4 0b0101
b0110 = W4 0b0110
b0111 = W4 0b0111
b1000 = W4 0b1000
b1001 = W4 0b1001
b1010 = W4 0b1010
b1011 = W4 0b1011
b1100 = W4 0b1100
b1101 = W4 0b1101
b1110 = W4 0b1110
b1111 = W4 0b1111

pattern B0000 :: Word4
pattern B0000 = W4 0b0000

-- | 'Word4' smart constructor dropping upper bits
leastSignificant4 :: Integral integral => integral -> Word4
leastSignificant4 = W4 . (0xF .&.) . fromIntegral

newtype Word6 = W6 Word8
    deriving (Eq, Ord, Show)

-- | 'Word6' smart constructor dropping upper bits
leastSignificant6 :: Integral integral => integral -> Word6
leastSignificant6 = W6 . (0x3F .&.) . fromIntegral

newtype Word60 = W60 Word64
    deriving (Eq, Ord, Show)

-- | 'Word60' smart constructor dropping upper bits
leastSignificant60 :: Integral integral => integral -> Word60
leastSignificant60 = W60 . (0x0FFFFFFFFFFFFFFF .&.) . fromIntegral

-- | 'Word60' smart constructor checking domain
toWord60 :: Word64 -> Maybe Word60
toWord60 w
    | w < 0x1000000000000000 = Just $ W60 w
    | otherwise              = Nothing

class SafeCast v w where
    safeCast :: v -> w

instance SafeCast Word2  Word4   where safeCast = coerce
instance SafeCast Word2  Word64  where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word4  Int     where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word4  Word64  where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word4  Word8   where safeCast = coerce
instance SafeCast Word6  Int     where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word6  Word8   where safeCast = coerce
instance SafeCast Word6  Word60  where safeCast = coerce @Word64
                                                . fromIntegral @Word8 . coerce
instance SafeCast Word6  Word64  where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word8  Word32  where safeCast = fromIntegral
instance SafeCast Word8  Word64  where safeCast = fromIntegral
instance SafeCast Word60 Word64  where safeCast = coerce
instance SafeCast Word64 Integer where safeCast = fromIntegral

-- Fixed are Integers inside, so have arbitrary magnitude
instance HasResolution e => SafeCast Word64 (Fixed e) where
    safeCast = fromIntegral
