{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RON.Util.Word (
    -- * Word2
    Word2,
    b00, b01, b10, b11,
    pattern B00, pattern B01, pattern B10, pattern B11,
    leastSignificant2,
    -- * Word4
    Word4,
    b0000, b0001, b0010, b0011, b0100, b0101, b0110, b0111,
    b1000, b1001, b1010, b1011, b1100, b1101, b1110, b1111,
    pattern B0000,
    leastSignificant4,
    -- * Word6
    Word6 (..),
    leastSignificant6,
    ls6,
    -- * Word8
    Word8,
    -- * Word12
    Word12,
    leastSignificant12,
    ls12,
    -- * Word16
    Word16,
    -- * Word24
    Word24,
    leastSignificant24,
    ls24,
    -- * Word32
    Word32,
    -- * Word60
    Word60,
    leastSignificant60,
    ls60,
    toWord60,
    -- * Word64
    Word64,
    -- * SafeCast
    SafeCast (..),
) where

import           RON.Prelude

import           Data.Bits ((.&.))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Hashable (hashUsing, hashWithSalt)
import           GHC.Num (abs, fromInteger, signum)

newtype Word2 = W2 Word8
    deriving (Eq, Hashable, Ord, Show)

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

-- | 'leastSignificant6' specialized for 'Word8'
ls6 :: Word8 -> Word6
ls6 = W6 . (0x3F .&.)

newtype Word12 = W12 Word16
    deriving (Eq, Ord, Show)

-- | 'Word12' smart constructor dropping upper bits
leastSignificant12 :: Integral integral => integral -> Word12
leastSignificant12 = W12 . (0xFFF .&.) . fromIntegral

-- | 'leastSignificant12' specialized for 'Word16'
ls12 :: Word16 -> Word12
ls12 = W12 . (0xFFF .&.)

newtype Word24 = W24 Word32
    deriving (Eq, Ord, Show)

-- | 'Word24' smart constructor dropping upper bits
leastSignificant24 :: Integral integral => integral -> Word24
leastSignificant24 = W24 . (0xFFFFFF .&.) . fromIntegral

-- | 'leastSignificant24' specialized for 'Word32'
ls24 :: Word32 -> Word24
ls24 = W24 . (0xFFF .&.)

newtype Word60 = W60 Word64
    deriving (Enum, Eq, Ord, Show)

instance Num Word60 where
    W60 x + W60 y = ls60 $ x + y
    W60 x * W60 y = ls60 $ x * y
    abs = id
    signum (W60 x) = W60 $ signum x
    fromInteger = leastSignificant60
    negate (W60 x) = W60 $ 0x1000000000000000 - x

instance Bounded Word60 where
    minBound = W60 0
    maxBound = W60 0xFFFFFFFFFFFFFFF

instance Hashable Word60 where
    hashWithSalt = hashUsing @Word64 coerce

-- | 'Word60' smart constructor dropping upper bits
leastSignificant60 :: Integral integral => integral -> Word60
leastSignificant60 = W60 . (0xFFFFFFFFFFFFFFF .&.) . fromIntegral

-- | 'leastSignificant60' specialized for 'Word64'
ls60 :: Word64 -> Word60
ls60 = W60 . (0xFFFFFFFFFFFFFFF .&.)

-- | 'Word60' smart constructor checking domain
toWord60 :: Word64 -> Maybe Word60
toWord60 w
    | w < 0x1000000000000000 = Just $ W60 w
    | otherwise              = Nothing

class SafeCast v w where
    safeCast :: v -> w

instance SafeCast Word2  Int     where safeCast = fromIntegral @Word8 . coerce
instance SafeCast Word2  Word4   where safeCast = coerce
instance SafeCast Word2  Word8   where safeCast = coerce
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
instance SafeCast Word12 Word64  where safeCast = fromIntegral @Word16 . coerce
instance SafeCast Word24 Word64  where safeCast = fromIntegral @Word32 . coerce
instance SafeCast Word24 Word32  where safeCast = coerce
instance SafeCast Word60 Word64  where safeCast = coerce
instance SafeCast Word64 Integer where safeCast = fromIntegral

-- Fixed are Integers inside, so have arbitrary magnitude
instance HasResolution e => SafeCast Word64 (Fixed e) where
    safeCast = fromIntegral
