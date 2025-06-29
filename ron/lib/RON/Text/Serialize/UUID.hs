{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Text.Serialize.UUID (
    serializeUuid,
    serializeUuidAtom,
    serializeUuidKey,
    uuidToString,
    uuidToText,
) where

import RON.Prelude

import Data.Bits (countLeadingZeros, shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Foldable (minimumBy)
import qualified Data.Text as Text

import qualified RON.Base64 as Base64
import RON.UUID (UUID (..), UuidFields (..), split, zero)
import RON.Util.Word (
    Word2,
    Word60,
    ls60,
    safeCast,
    pattern B00,
    pattern B0000,
    pattern B01,
    pattern B10,
    pattern B11,
 )

-- | Serialize UUID without context (used for test)
serializeUuid :: UUID -> ByteStringL
serializeUuid this =
    BSL.fromStrict $ case uuidVariant of
        B00 -> unzipped thisFields
        _ -> serializeUuidGeneric this
  where
    thisFields@UuidFields{..} = split this

uuidToString :: UUID -> String
uuidToString = BSLC.unpack . serializeUuid

uuidToText :: UUID -> Text
uuidToText = Text.pack . uuidToString

-- | Serialize UUID in op key context
serializeUuidKey ::
    -- | same key in the previous op (default is 'zero')
    UUID ->
    -- | previous key of the same op (default is 'zero')
    UUID ->
    -- | this
    UUID ->
    ByteStringL
serializeUuidKey prevKey prev this =
    BSL.fromStrict $ case thisFields.uuidVariant of
        B00 ->
            minimumByLength $
                unzipped thisFields
                    : zipIfDefaultVariant prevKey this
                    ++ [ "`" <> z
                       | prev /= zero
                       , z <- zipIfDefaultVariant prev this
                       ]
        _ -> serializeUuidGeneric this
  where
    thisFields = split this

-- | Serialize UUID in op value (atom) context
serializeUuidAtom ::
    -- | previous
    UUID ->
    -- | this
    UUID ->
    ByteStringL
serializeUuidAtom prev this =
    BSL.fromStrict $ case thisFields.uuidVariant of
        B00 ->
            minimumByLength $
                unzipped thisFields
                    : (guard (prev /= zero) *> zipIfDefaultVariant prev this)
        _ -> serializeUuidGeneric this
  where
    thisFields = split this

zipIfDefaultVariant :: UUID -> UUID -> [ByteString]
zipIfDefaultVariant prev this =
    [ z
    | (split prev).uuidVariant == B00
    , Just z <- [zipUuid (split prev) (split this)]
    ]

unzipped :: UuidFields -> ByteString
unzipped UuidFields{..} = x' <> y'
  where
    variety = case uuidVariety of
        B0000 -> ""
        _ -> BS.singleton (Base64.encodeLetter4 uuidVariety) <> "/"
    x' = variety <> Base64.encode60short uuidValue
    y' = case (uuidVersion, uuidOrigin) of
        (B00, safeCast -> 0 :: Word64) -> ""
        _ ->
            serializeVersion uuidVersion
                `BSC.cons` Base64.encode60short uuidOrigin

zipUuid :: UuidFields -> UuidFields -> Maybe ByteString
zipUuid prev this
    | prev == this = Just ""
    | canReuseValue = valueZip
    | otherwise = Nothing
  where
    canReuseValue = prev{uuidValue = this.uuidValue} == this
    valueZip = zipPrefix prev.uuidValue this.uuidValue

zipPrefix :: Word60 -> Word60 -> Maybe ByteString
zipPrefix prev this
    | commonBits >= 6 * 10 = pure ""
    | commonBits >= 6 * 9 = ok ')' 9
    | commonBits >= 6 * 8 = ok ']' 8
    | commonBits >= 6 * 7 = ok '}' 7
    | commonBits >= 6 * 6 = ok '{' 6
    | commonBits >= 6 * 5 = ok '[' 5
    | commonBits >= 6 * 4 = ok '(' 4
    | otherwise = Nothing
  where
    ok c n = pure $ BSC.cons c $ encode60short' $ safeCast this `shiftL` (6 * n)
    commonBits =
        countLeadingZeros (safeCast prev `xor` safeCast this :: Word64) - 4
    encode60short' = \case
        0 -> ""
        w -> Base64.encode60short $ ls60 w

serializeVersion :: Word2 -> Char
serializeVersion = \case
    B00 -> '$'
    B01 -> '%'
    B10 -> '+'
    B11 -> '-'

serializeUuidGeneric :: UUID -> ByteString
serializeUuidGeneric (UUID x y) = Base64.encode64 x <> Base64.encode64 y

-- | XXX Partial for lists!
minimumByLength :: (Foldable f) => f ByteString -> ByteString
minimumByLength = minimumBy $ comparing BS.length
