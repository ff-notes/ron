{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Text.Serialize.UUID
    ( serializeUuid
    , serializeUuidAtom
    , serializeUuidKey
    -- TODO temporarily unused
    , minimumByLength
    , zero
    , zipUuid
    ) where

import           RON.Internal.Prelude

import           Data.Bits (countLeadingZeros, shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (ord)
import           Data.List (minimumBy)
import           Data.Ord (comparing)

import qualified RON.Base64 as Base64
import           RON.Internal.Word (pattern B00, pattern B0000, pattern B01,
                                    pattern B10, pattern B11, Word2, Word4,
                                    Word60, ls60, safeCast)
import           RON.UUID (UUID (..), UuidFields (..), split, zero)

serializeUuid :: UUID -> ByteStringL
serializeUuid this =
    BSL.fromStrict $ case uuidVariant of
        B00 -> unzipped thisFields
        _   -> serializeUuidGeneric this
  where
    thisFields@UuidFields{..} = split this

serializeUuidKey :: UUID -> UUID -> UUID -> ByteStringL
serializeUuidKey _prevKey _prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 ->
            -- minimumByLength
            unzipped thisFields
            -- :   [ zipUuid False (split prevKey) thisFields
            --     | uuidVariant (split prevKey) == B00 ]
            -- ++  [ "`" <> zipUuid True (split prev) thisFields
            --     | prev /= zero, uuidVariant (split prev) == B00 ]
        _   -> serializeUuidGeneric this
  where
    thisFields = split this

serializeUuidAtom :: UUID -> UUID -> ByteStringL
serializeUuidAtom _prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 ->
            -- minimumByLength
            unzipped thisFields
            -- :   [ zipUuid False (split prev) thisFields
            --     | prev /= zero, uuidVariant (split prev) == B00 ]
        _   -> serializeUuidGeneric this
  where
    thisFields = split this

unzipped :: UuidFields -> ByteString
unzipped UuidFields{..} = x' <> y'
  where
    variety = case uuidVariety of
        B0000 -> ""
        _     -> BS.singleton (Base64.encodeLetter4 uuidVariety) <> "/"
    x' = variety <> Base64.encode60short uuidValue
    y' = case (uuidScheme, uuidOrigin) of
        (B00, safeCast -> 0 :: Word64) -> ""
        _ ->
            serializeScheme uuidScheme
            `BSC.cons` Base64.encode60short uuidOrigin

zipUuid :: Bool -> UuidFields -> UuidFields -> ByteString
zipUuid changeZipBase prev this
    | prev == this = ""
    | otherwise    = mconcat [variety, val, scheme, ori]
  where
    val = fromMaybe value  valueZip
    ori = fromMaybe origin originZip
    canReuseWholeParts
        =   changeZipBase
        ||  isJust valueZip
        ||  isJust originZip
        ||  (   uuidScheme prev /= uuidScheme this
            &&  uuidOrigin prev == uuidOrigin this
            )
    variety
        | canReuseWholeParts && uuidVariety prev == uuidVariety this = ""
        | otherwise = serializeVariety $ uuidVariety this
    value
        | canReuseWholeParts && uuidValue prev == uuidValue this = ""
        | otherwise = Base64.encode60short $ uuidValue this
    valueZip = zipPrefix (uuidValue prev) (uuidValue this)
    scheme
        |   canReuseWholeParts
            &&  uuidScheme prev == uuidScheme this
            &&  uuidOrigin prev == uuidOrigin this
                = ""
        | otherwise = BSC.singleton $ serializeScheme $ uuidScheme this
    origin
        | canReuseWholeParts && uuidOrigin prev == uuidOrigin this = ""
        | otherwise = Base64.encode60short $ uuidOrigin this
    originZip = zipPrefix (uuidOrigin prev) (uuidOrigin this)

zipPrefix :: Word60 -> Word60 -> Maybe ByteString
zipPrefix prev this
    | commonBits >= 6 * 10 = pure ""
    | commonBits >= 6 *  9 = ok ')' 9
    | commonBits >= 6 *  8 = ok ']' 8
    | commonBits >= 6 *  7 = ok '}' 7
    | commonBits >= 6 *  6 = ok '{' 6
    | commonBits >= 6 *  5 = ok '[' 5
    | commonBits >= 6 *  4 = ok '(' 4
    | otherwise        = Nothing
  where
    ok c n = pure $ BSC.cons c $ encode60short' $ safeCast this `shiftL` (6 * n)
    commonBits =
        countLeadingZeros (safeCast prev `xor` safeCast this :: Word64) - 4
    encode60short' = \case
        0 -> ""
        w -> Base64.encode60short $ ls60 w

serializeScheme :: Word2 -> Char
serializeScheme = \case
    B00 -> '$'
    B01 -> '%'
    B10 -> '+'
    B11 -> '-'

serializeVariety :: Word4 -> ByteString
serializeVariety = \case
    B0000 -> ""
    v     -> BS.pack [Base64.encodeLetter4 v, fromIntegral $ ord '/']

serializeUuidGeneric :: UUID -> ByteString
serializeUuidGeneric (UUID x y) = Base64.encode64 x <> Base64.encode64 y

minimumByLength :: Foldable f => f ByteString -> ByteString
minimumByLength = minimumBy $ comparing BS.length
