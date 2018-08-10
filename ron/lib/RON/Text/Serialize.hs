{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Serialize
    ( serializeFrame
    , serializeFrames
    , serializeOp
    , serializeUuid
    ) where

import           RON.Internal.Prelude

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified RON.Base64 as Base64
import           RON.Internal.Word (pattern B00, pattern B0000, pattern B01,
                                    pattern B10, pattern B11)
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
    Raw op      -> serializeOp op `BSLC.snoc` ';'
    State chunk -> serializeReducedChunk False chunk
    Query chunk -> serializeReducedChunk True  chunk

serializeReducedChunk :: Bool -> ReducedChunk -> ByteStringL
serializeReducedChunk isQuery ReducedChunk{chunkHeader, chunkBody} =
    mconcat
        [ serializeOp chunkHeader
        , if isQuery then "?" else "!"
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
        B00 -> x' <> y'
        _   -> Base64.encode64 x <> Base64.encode64 y
  where
    UuidFields{..} = UUID.split u
    schemeSymbol = case uuidScheme of
        B00 -> '$'
        B01 -> '%'
        B10 -> '+'
        B11 -> '-'
    varietyPrefix = case uuidVariety of
        B0000 -> ""
        _     -> BS.singleton (Base64.encodeLetter4 uuidVariety) <> "/"
    x' = varietyPrefix <> Base64.encode60short uuidValue
    y' = case y of
        0 -> ""
        _ -> BSC.singleton schemeSymbol <> Base64.encode60short uuidOrigin

serializeAtom :: Atom -> ByteStringL
serializeAtom = \case
    AInteger i -> "=" <> BSLC.pack (show i)
    AString  s -> Json.encode s
    AUuid    u -> ">" <> serializeUuid u
