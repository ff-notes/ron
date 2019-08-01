-- Copyright (c) 2016, Pasqualino `Titto` Assini

module Data.ZigZag (
    zzEncode,
    zzDecode64,
) where

import           RON.Prelude

import           Data.Bits (Bits, FiniteBits, finiteBitSize, shiftL, shiftR,
                            xor, (.&.))

{-# SPECIALIZE INLINE zzEncode :: Int8 -> Word8 #-}
{-# SPECIALIZE INLINE zzEncode :: Int16 -> Word16 #-}
{-# SPECIALIZE INLINE zzEncode :: Int32 -> Word32 #-}
{-# SPECIALIZE INLINE zzEncode :: Int64 -> Word64 #-}
zzEncode :: (Num b, Integral a, FiniteBits a) => a -> b
zzEncode w = fromIntegral ((w `shiftL` 1) `xor` (w `shiftR` (finiteBitSize w -1)))

{-# INLINE zzDecode #-}
zzDecode :: (Num a, Integral a1, Bits a1) => a1 -> a
zzDecode w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))

{-# INLINE zzDecode64 #-}
zzDecode64 :: Word64 -> Int64
zzDecode64 = zzDecode
