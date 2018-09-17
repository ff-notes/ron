-- Copyright (c) 2016, Pasqualino `Titto` Assini

module Data.ZigZag
    ( zzEncode
    , zzEncodeInteger
    , zzDecode8
    , zzDecode16
    , zzDecode32
    , zzDecode64
    , zzDecodeInteger
    , zzDecode
    ) where

import Data.Word
import Data.Int
import Data.Bits

{-# SPECIALIZE INLINE zzEncode :: Int8 -> Word8 #-}
{-# SPECIALIZE INLINE zzEncode :: Int16 -> Word16 #-}
{-# SPECIALIZE INLINE zzEncode :: Int32 -> Word32 #-}
{-# SPECIALIZE INLINE zzEncode :: Int64 -> Word64 #-}
zzEncode :: (Num b, Integral a, FiniteBits a) => a -> b
zzEncode w = fromIntegral ((w `shiftL` 1) `xor` (w `shiftR` (finiteBitSize w -1)))

--{-# INLINE zzEncode8 #-}
--zzEncode8 :: Int8 -> Word8
-- zzEncode8 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 7))

-- {-# INLINE zzEncode16 #-}
-- zzEncode16 :: Int16 -> Word16
-- zzEncode16 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 15))

-- {-# INLINE zzEncode32 #-}
-- zzEncode32 :: Int32 -> Word32
-- zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))

-- {-# INLINE zzEncode64 #-}
-- zzEncode64 :: Int64 -> Word64
-- zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))

{-# INLINE zzEncodeInteger #-}
zzEncodeInteger :: Integer -> Integer
zzEncodeInteger x | x>=0      = x `shiftL` 1
                  | otherwise = negate (x `shiftL` 1) - 1

-- {-# SPECIALIZE INLINE zzDecode :: Word8 -> Int8 #-}
-- {-# SPECIALIZE INLINE zzDecode :: Word16 -> Int16 #-}
-- {-# SPECIALIZE INLINE zzDecode :: Word32 -> Int32 #-}
-- {-# SPECIALIZE INLINE zzDecode :: Word64 -> Int64 #-}
-- {-# SPECIALIZE INLINE zzDecode :: Integer -> Integer #-}

{-# INLINE zzDecode #-}
zzDecode :: (Num a, Integral a1, Bits a1) => a1 -> a
zzDecode w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))
-- zzDecode w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecode8 #-}
zzDecode8 :: Word8 -> Int8
zzDecode8 = zzDecode

{-# INLINE zzDecode16 #-}
zzDecode16 :: Word16 -> Int16
zzDecode16 = zzDecode

{-# INLINE zzDecode32 #-}
zzDecode32 :: Word32 -> Int32
zzDecode32 = zzDecode

{-# INLINE zzDecode64 #-}
zzDecode64 :: Word64 -> Int64
zzDecode64 = zzDecode

{-# INLINE zzDecodeInteger #-}
zzDecodeInteger :: Integer -> Integer
zzDecodeInteger = zzDecode
