{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Serialize
    ( serializeFrame
    , serializeFrames
    , serializeOp
    , serializeUuid
    ) where

import           Internal.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified RON.Base64 as Base64
import           RON.Types (Frame, Op (..))
import           RON.UUID (UUID (..), UuidFields (..))
import qualified RON.UUID as UUID

serializeFrame :: Frame -> ByteStringL
serializeFrame = (`BSLC.snoc` '.') . foldMap serializeOp

serializeFrames :: [Frame] -> ByteStringL
serializeFrames = foldMap serializeFrame

serializeOp :: Op -> ByteStringL
serializeOp Op{..} =
    let typ      = serializeUuid opType
        object   = serializeUuid opObject
        event    = serializeUuid opEvent
        location = serializeUuid opLocation
    in  "*" <> typ <> " #" <> object <> " @" <> event <> " :" <> location <>
        "!\n"

serializeUuid :: UUID -> ByteStringL
serializeUuid u@(UUID x y) = BSL.fromStrict $
    case uuidVariant of
        0b00 -> x' <> y'
        _    -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{..} = UUID.split u
    schemeSymbol = case uuidScheme of
        0b00 -> '$'
        0b01 -> '%'
        0b10 -> '+'
        _    -> '-'
    varietyPrefix = case uuidVariety of
        0 -> ""
        _ -> BS.singleton (Base64.encodeLetter uuidVariety) <> "/"
    x' = varietyPrefix <> Base64.encode60short uuidValue
    y' = case y of
        0 -> ""
        _ -> BSC.singleton schemeSymbol <> Base64.encode60short uuidOrigin
