{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Base64
    ( decode
    , decode60
    , decode64
    , decodeLetter
    , decodeLetter4
    , encode
    , encode60
    , encode60short
    , encode64
    , encodeLetter
    , encodeLetter4
    , isLetter
    ) where

import           RON.Internal.Prelude

import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (isAlphaNum, ord)

import           RON.Internal.Word (Word4, Word6, Word60, leastSignificant4,
                                    leastSignificant6, leastSignificant60,
                                    safeCast)

alphabet :: ByteString
alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"

-- | TODO use approach from 'isUpperHexDigit'
isLetter :: Char -> Bool
isLetter c = isAlphaNum c || c == '_' || c == '~'

decodeLetter :: Word8 -> Maybe Word6
decodeLetter x
    | x <  ord0 = Nothing
    | x <= ord9 = Just . leastSignificant6 $ x - ord0
    | x <  ordA = Nothing
    | x <= ordZ = Just . leastSignificant6 $ x - ordA + posA
    | x == ord_ = Just $ leastSignificant6 pos_
    | x <  orda = Nothing
    | x <= ordz = Just . leastSignificant6 $ x - orda + posa
    | x == ordõ = Just $ leastSignificant6 posõ
    | otherwise  = Nothing

ord0, ord9, ordA, ordZ, ord_, orda, ordz, ordõ :: Word8
ord0 = fromIntegral $ ord '0'
ord9 = fromIntegral $ ord '9'
ordA = fromIntegral $ ord 'A'
ordZ = fromIntegral $ ord 'Z'
ord_ = fromIntegral $ ord '_'
orda = fromIntegral $ ord 'a'
ordz = fromIntegral $ ord 'z'
ordõ = fromIntegral $ ord '~'

posA, pos_, posa, posõ :: Word8
posA = 10
pos_ = 36
posa = 37
posõ = 63

decodeLetter4 :: Word8 -> Maybe Word4
decodeLetter4 x
    | x <  ord0 = Nothing
    | x <= ord9 = Just . leastSignificant4 $ x - ord0
    | x <  ordA = Nothing
    | x <= ordZ = Just . leastSignificant4 $ x - ordA + posA
    | otherwise  = Nothing

decode :: ByteStringL -> Maybe ByteStringL
decode =
    fmap (BSL.pack . go . map safeCast) . traverse decodeLetter . BSL.unpack
  where
    go = \case
        [a, b]       -> decode2 a b
        [a, b, c]    -> decode3 a b c
        a:b:c:d:rest -> decode4 a b c d ++ go rest
        _            -> []
    decode2 a b = [(a `shiftL` 2) .|. (b `shiftR` 4)]
    decode3 a b c =
        [ ( a             `shiftL` 2) .|. (b `shiftR` 4)
        , ((b .&. 0b1111) `shiftL` 4) .|. (c `shiftR` 2)
        ]
    decode4 a b c d =
        [ ( a             `shiftL` 2) .|. (b `shiftR` 4)
        , ((b .&. 0b1111) `shiftL` 4) .|. (c `shiftR` 2)
        , ((c .&.   0b11) `shiftL` 6) .|.  d
        ]

decode60 :: ByteString -> Maybe Word60
decode60 =
    fmap leastSignificant60 . go 10
    <=< traverse (fmap safeCast . decodeLetter) . BS.unpack
  where
    go :: Int -> [Word8] -> Maybe Word64
    go n
        | n > 0 = \case
            []           -> Just 0
            [a]          -> Just $ decode4 a 0 0 0
            [a, b]       -> Just $ decode4 a b 0 0
            [a, b, c]    -> Just $ decode4 a b c 0
            a:b:c:d:rest -> do
                lowerPart <- go (n - 4) rest
                pure $ decode4 a b c d .|. (lowerPart `shiftR` 24)
        | otherwise = \case
            [] -> Just 0
            _  -> Nothing  -- extra input
    decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> Word64
    decode4 a b c d =
        (safeCast a `shiftL` 54) .|.
        (safeCast b `shiftL` 48) .|.
        (safeCast c `shiftL` 42) .|.
        (safeCast d `shiftL` 36)

decode64 :: ByteString -> Maybe Word64
decode64 s = do
    (s0, s1) <- BS.uncons s
    v <- decodeLetter4 s0
    w <- decode60 s1
    pure $ cons v w
  where
    cons :: Word4 -> Word60 -> Word64
    cons v w = (safeCast v `shiftL` 60) .|. safeCast w

encode :: ByteStringL -> ByteStringL
encode = BSL.pack . go . BSL.unpack
  where
    go = \case
        []         -> []
        [a]        -> encode1 a
        [a, b]     -> encode2 a b
        a:b:c:rest -> encode3 a b c ++ go rest
    encode1 a =
        map (encodeLetter . leastSignificant6)
            [a `shiftR` 2, (a .&. 0b11) `shiftL` 4]
    encode2 a b = map (encodeLetter . leastSignificant6)
        [                                  a `shiftR` 2
        , ((a .&.   0b11) `shiftL` 4) .|. (b `shiftR` 4)
        ,  (b .&. 0b1111) `shiftL` 2
        ]
    encode3 a b c = map (encodeLetter . leastSignificant6)
        [                                    a `shiftR` 2
        , ((a .&.     0b11) `shiftL` 4) .|. (b `shiftR` 4)
        , ((b .&.   0b1111) `shiftL` 2) .|. (c `shiftR` 6)
        ,   c .&. 0b111111
        ]

encodeLetter :: Word6 -> Word8
encodeLetter i = alphabet `BS.index` safeCast i

encodeLetter4 :: Word4 -> Word8
encodeLetter4 i = alphabet `BS.index` safeCast i

encode60 :: Word60 -> ByteString
encode60 w = BS.pack $
    map (encodeLetter . leastSignificant6)
        [ (safeCast w `shiftR` (6 * i)) .&. 0b111111 :: Word64
        | i <- [9, 8 .. 0]
        ]

-- | Encode Word60, dropping trailing zeroes
encode60short :: Word60 -> ByteString
encode60short v = case safeCast v :: Word64 of
    0 -> "0"
    x -> BS.pack . map (encodeLetter . leastSignificant6) $ go 9 x
  where
    go _ 0 = []
    go i w =
        (w `shiftR` (6 * i)) .&. 0b111111 :
        go (i - 1) (w .&. complement (0b111111 `shiftL` (6 * i)))

encode64 :: Word64 -> ByteString
encode64 w =
    encodeLetter (leastSignificant6 $ w `shiftR` 60)
    `BS.cons` encode60 (leastSignificant60 w)
