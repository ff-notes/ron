{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module RON.Types
    ( Atom (..)
    , Chunk (..)
    , Frame
    , Op (..)
    , opEvent
    , opLocation
    , opPayload
    , OpTerm (..)
    , RChunk (..)
    , ROp (..)
    , UUID (..)
    , valueChunk
    , valueFrame
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Data, Eq, Generic, Hashable, NFData, Show)

data ROp = ROp
    { ropEvent    :: UUID
    , ropLocation :: UUID
    , ropPayload  :: [Atom]
    }
    deriving (Data, Eq, Generic, Hashable, NFData, Show)

data Op = Op
    { opType   :: UUID
    , opObject :: UUID
    , opR      :: ROp
    }
    deriving (Data, Eq, Generic, NFData)

instance Show Op where
    show Op{opType, opObject, opR = ROp{ropEvent, ropLocation, ropPayload}} =
        unwords
            [ "Op"
            , insert '*' $ show opType
            , insert '#' $ show opObject
            , insert '@' $ show ropEvent
            , insert ':' $ show ropLocation
            , show ropPayload
            ]
      where
        insert k = \case
            []   -> [k]
            c:cs -> c:k:cs

data RChunk = RChunk
    { chunkHeader :: Op
    , chunkBody   :: [Op]
    }
    deriving (Data, Eq, Generic, NFData, Show)

data Chunk = Raw Op | Value RChunk | Query RChunk
    deriving (Data, Eq, Generic, NFData, Show)

type Frame = [Chunk]

data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

valueChunk :: Op -> [Op] -> Chunk
valueChunk chunkHeader chunkBody = Value RChunk{chunkHeader, chunkBody}

valueFrame :: Op -> [Op] -> Frame
valueFrame chunkHeader chunkBody = [valueChunk chunkHeader chunkBody]

opEvent :: Op -> UUID
opEvent = ropEvent . opR

opLocation :: Op -> UUID
opLocation = ropLocation . opR

opPayload :: Op -> [Atom]
opPayload = ropPayload . opR
