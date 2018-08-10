{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Serialize
    ( serializeFrame
    , serializeFrames
    , serializeOp
    , serializeUuid
    ) where

import           RON.Internal.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified RON.Base64 as Base64
import           RON.Types (Atom (..), Chunk (..), Frame, Op (..),
                            ReducedChunk (..))
import           RON.UUID (UUID (..), UuidFields (..))
import qualified RON.UUID as UUID

serializeFrame :: Frame -> ByteStringL
serializeFrame = (`BSLC.snoc` '.') . foldMap serializeChunk

serializeFrames :: [Frame] -> ByteStringL
serializeFrames = foldMap serializeFrame

serializeChunk :: Chunk -> ByteStringL
serializeChunk = \case
    Raw op        -> serializeOp op `BSLC.snoc` ';'
    Reduced chunk -> serializeReducedChunk chunk

serializeReducedChunk :: ReducedChunk -> ByteStringL
serializeReducedChunk ReducedChunk{chunkHeader, chunkIsQuery, chunkBody} =
    mconcat
        [ serializeOp chunkHeader
        , if chunkIsQuery then "?" else "!"
        , foldMap serializeOp chunkBody
        ]

serializeOp :: Op -> ByteStringL
serializeOp Op{..} =
    let typ      =     serializeUuid opType
        object   =     serializeUuid opObject
        event    =     serializeUuid opEvent
        location =     serializeUuid opLocation
        payload  = map serializeAtom opPayload
    in  BSLC.unwords $
            "*" <> typ : "#" <> object : "@" <> event : ":" <> location :
            payload

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

serializeAtom :: Atom -> ByteStringL
serializeAtom = \case
    AUuid    u   -> ">" <> serializeUuid u
    AInteger int -> "=" <> BSLC.pack (show int)
