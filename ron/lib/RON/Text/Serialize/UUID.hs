{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

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
                                    pattern B10, pattern B11, Word2)
import           RON.UUID (UUID (..), UuidFields (..), zero)
import qualified RON.UUID as UUID

serializeUuid :: UUID -> ByteStringL
serializeUuid this@(UUID x y) =
    BSL.fromStrict $ case uuidVariant of
        B00 -> unzipped this
        _   -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{..} = UUID.split this

serializeUuidKey :: UUID -> UUID -> UUID -> ByteStringL
serializeUuidKey prevKey prev this@(UUID x y) =
    BSL.fromStrict $ case uuidVariant of
        B00 -> minimumBy (comparing BS.length)
            $   [unzipped this]
            ++  [ z
                | prevKey /= zero || this == zero
                , Just z <- [zipUuid prevKey this]
                ]
            ++  ["`" <> z | prev /= zero, Just z <- [zipUuid prev this]]
        _   -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{..} = UUID.split this

serializeUuidAtom :: UUID -> UUID -> ByteStringL
serializeUuidAtom prev this@(UUID x y) =
    BSL.fromStrict $ case uuidVariant of
        B00 -> minimumBy (comparing BS.length)
            $ unzipped this
            : ["`" <> z | prev /= zero, Just z <- [zipUuid prev this]]
        _   -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{..} = UUID.split this

unzipped :: UUID -> ByteString
unzipped this@(UUID _ y) = x' <> y'
  where
    UuidFields{..} = UUID.split this
    varietyPrefix = case uuidVariety of
        B0000 -> ""
        _     -> BS.singleton (Base64.encodeLetter4 uuidVariety) <> "/"
    x' = varietyPrefix <> Base64.encode60short uuidValue
    y' = case y of
        0 -> ""
        _ -> serializeScheme uuidScheme `BSC.cons` Base64.encode60short uuidOrigin

zipUuid :: UUID -> UUID -> Maybe ByteString
zipUuid base this
    | this == base = Just ""
    | otherwise    = Nothing

serializeScheme :: Word2 -> Char
serializeScheme = \case
    B00 -> '$'
    B01 -> '%'
    B10 -> '+'
    B11 -> '-'
