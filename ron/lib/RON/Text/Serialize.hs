{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
serializeOp Op{typ, object, event, location} =
    let t = serializeUuid typ
        o = serializeUuid object
        e = serializeUuid event
        r = serializeUuid location
    in "*" <> t <> " #" <> o <> " @" <> e <> " :" <> r <> "!\n"

serializeUuid :: UUID -> ByteStringL
serializeUuid u@(UUID x y) = BSL.fromStrict $
    case variant of
        0b00 -> x' <> y'
        _    -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{variety, value, variant, scheme, origin} = UUID.split u
    schemeSymbol = case scheme of
        0b00 -> '$'
        0b01 -> '%'
        0b10 -> '+'
        _    -> '-'
    varietyPrefix = case variety of
        0 -> ""
        _ -> BS.singleton (Base64.encodeLetter variety) <> "/"
    x' = varietyPrefix <> Base64.encode60short value
    y' = case y of
        0 -> ""
        _ -> BSC.singleton schemeSymbol <> Base64.encode60short origin
