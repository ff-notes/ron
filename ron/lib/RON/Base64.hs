{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Base64
    ( decode
    , decode60
    , decode64
    , decodeLetter
    , encode
    , encode60
    , encode60short
    , encode64
    , encodeLetter
    , isLetter
    ) where

import           RON.Internal.Prelude

import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (isAlphaNum, ord)

alphabet :: ByteString
alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"

-- | TODO use approach from 'isUpperHexDigit'
isLetter :: Char -> Bool
isLetter c = isAlphaNum c || c == '_' || c == '~'

decodeLetter :: Word8 -> Maybe Word6
decodeLetter x
    | x <  ord'0 = Nothing
    | x <= ord'9 = Just $ x - ord'0
    | x <  ord'A = Nothing
    | x <= ord'Z = Just $ x - ord'A + pos'A
    | x == ord'_ = Just pos'_
    | x <  ord'a = Nothing
    | x <= ord'z = Just $ x - ord'a + pos'a
    | x == ord'õ = Just pos'õ
    | otherwise  = Nothing
  where
    ord'0 = fromIntegral $ ord '0'
    ord'9 = fromIntegral $ ord '9'
    ord'A = fromIntegral $ ord 'A'
    ord'Z = fromIntegral $ ord 'Z'
    ord'_ = fromIntegral $ ord '_'
    ord'a = fromIntegral $ ord 'a'
    ord'z = fromIntegral $ ord 'z'
    ord'õ = fromIntegral $ ord '~'
    pos'A = 10
    pos'_ = 36
    pos'a = 37
    pos'õ = 63

decode :: ByteStringL -> Maybe ByteStringL
decode = fmap (BSL.pack . go) . traverse decodeLetter . BSL.unpack
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
decode60 = go 10 <=< traverse decodeLetter . BS.unpack
  where
    go :: Int -> [Word6] -> Maybe Word60
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
    decode4 :: Word6 -> Word6 -> Word6 -> Word6 -> Word60
    decode4 a b c d =
        (fromIntegral a `shiftL` 54) .|.
        (fromIntegral b `shiftL` 48) .|.
        (fromIntegral c `shiftL` 42) .|.
        (fromIntegral d `shiftL` 36)

decode64 :: ByteString -> Maybe Word64
decode64 s = do
    (s0, s1) <- BS.uncons s
    v <- decodeLetter s0
    w <- decode60 s1
    pure $ cons v w
  where
    cons v w = (fromIntegral v `shiftL` 60) .|. w

encode :: ByteStringL -> ByteStringL
encode = BSL.pack . go . BSL.unpack
  where
    go = \case
        []         -> []
        [a]        -> encode1 a
        [a, b]     -> encode2 a b
        a:b:c:rest -> encode3 a b c ++ go rest
    encode1 a = map encodeLetter [a `shiftR` 2, (a .&. 0b11) `shiftL` 4]
    encode2 a b = map encodeLetter
        [                                  a `shiftR` 2
        , ((a .&.   0b11) `shiftL` 4) .|. (b `shiftR` 4)
        ,  (b .&. 0b1111) `shiftL` 2
        ]
    encode3 a b c = map encodeLetter
        [                                    a `shiftR` 2
        , ((a .&.     0b11) `shiftL` 4) .|. (b `shiftR` 4)
        , ((b .&.   0b1111) `shiftL` 2) .|. (c `shiftR` 6)
        ,   c .&. 0b111111
        ]

encodeLetter :: Word6 -> Word8
encodeLetter i = alphabet `BS.index` fromIntegral i

encode60 :: Word60 -> ByteString
encode60 w = BS.pack $
    map (encodeLetter . fromIntegral)
        [(w `shiftR` (6 * i)) .&. 0b111111 | i <- [9, 8 .. 0]]

-- | Encode Word60, dropping trailing zeroes
encode60short :: Word60 -> ByteString
encode60short = \case
    0 -> "0"
    x -> BS.pack . map (encodeLetter . fromIntegral) $ go 9 x
  where
    go _ 0 = []
    go i w =
        (w `shiftR` (6 * i)) .&. 0b111111 :
        go (i - 1) (w .&. complement (0b111111 `shiftL` (6 * i)))

encode64 :: Word64 -> ByteString
encode64 w = encodeLetter (fromIntegral $ w `shiftR` 60) `BS.cons` encode60 w
