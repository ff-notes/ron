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
    , Op' (..)
    , OpTerm (..)
    , RChunk (..)
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

data Op = Op
    { opType   :: UUID
    , opObject :: UUID
    , op'      :: Op'
    }
    deriving (Data, Eq, Generic, NFData)

data Op' = Op'
    { opEvent   :: UUID
    , opRef     :: UUID
    , opPayload :: [Atom]
    }
    deriving (Data, Eq, Generic, Hashable, NFData, Show)

instance Show Op where
    show Op{opType, opObject, op' = Op'{opEvent, opRef, opPayload}} =
        unwords
            [ "Op"
            , insert '*' $ show opType
            , insert '#' $ show opObject
            , insert '@' $ show opEvent
            , insert ':' $ show opRef
            , show opPayload
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
