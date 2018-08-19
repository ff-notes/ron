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
    ) where

import           RON.Internal.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.List (minimumBy)
import           Data.Ord (comparing)

import qualified RON.Base64 as Base64
import           RON.Internal.Word (pattern B00, pattern B0000, pattern B01,
                                    pattern B10, pattern B11, Word2, Word4,
                                    safeCast)
import           RON.UUID (UUID (..), UuidFields (..), split, zero)

serializeUuid :: UUID -> ByteStringL
serializeUuid this =
    BSL.fromStrict $ case uuidVariant of
        B00 -> unzipped thisFields
        _   -> serializeUuidGeneric this
  where
    thisFields@UuidFields{..} = split this

serializeUuidKey :: UUID -> UUID -> UUID -> ByteStringL
serializeUuidKey prevKey prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 -> minimumBy (comparing BS.length)
            $   unzipped thisFields
            :   [ z
                | prevKey /= zero || this == zero
                , uuidVariant (split prevKey) == B00
                , Just z <- [zipUuid False (split prevKey) thisFields]
                ]
            ++  [ "`" <> z
                | prev /= zero
                , uuidVariant (split prev) == B00
                , Just z <- [zipUuid True (split prev) thisFields]
                ]
        _   -> serializeUuidGeneric this
  where
    thisFields = split this

serializeUuidAtom :: UUID -> UUID -> ByteStringL
serializeUuidAtom prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 -> minimumBy (comparing BS.length)
            $   unzipped thisFields
            :   [ z
                | prev /= zero
                , uuidVariant (split prev) == B00
                , Just z <- [zipUuid False (split prev) thisFields]
                ]
            ++  [ BSC.cons '`' z
                | prev /= zero
                , uuidVariant (split prev) == B00
                , Just z <- [zipUuid True (split prev) thisFields]
                ]
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

zipUuid :: Bool -> UuidFields -> UuidFields -> Maybe ByteString
zipUuid changeZipBase prev this
    | prev == this       = Just ""
    | canReuseWholeParts = Just $ mconcat [variety, value, scheme, origin]
    | otherwise          = Nothing
  where
    canReuseWholeParts
        =   changeZipBase
        ||  (   uuidScheme prev /= uuidScheme this
            &&  uuidOrigin prev == uuidOrigin this
            )
    variety
        | uuidVariety prev == uuidVariety this = ""
        | otherwise = serializeVariety $ uuidVariety this
    value
        | uuidValue prev == uuidValue this = ""
        | otherwise = Base64.encode60short $ uuidValue this
    scheme
        |       uuidScheme prev == uuidScheme this
            &&  uuidOrigin prev == uuidOrigin this
                = ""
        | otherwise = BSC.singleton $ serializeScheme $ uuidScheme this
    origin
        | uuidOrigin prev == uuidOrigin this = ""
        | otherwise = Base64.encode60short $ uuidOrigin this

serializeScheme :: Word2 -> Char
serializeScheme = \case
    B00 -> '$'
    B01 -> '%'
    B10 -> '+'
    B11 -> '-'

serializeVariety :: Word4 -> ByteString
serializeVariety = \case
    B0000 -> ""
    v     -> BS.pack [Base64.encodeLetter4 v, 47] -- '/'

serializeUuidGeneric :: UUID -> ByteString
serializeUuidGeneric (UUID x y) = Base64.encode64 x <> Base64.encode64 y
