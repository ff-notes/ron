{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module RON.Types
    ( Atom (..)
    , Chunk (..)
    , Frame
    , Op (..)
    , OpTerm (..)
    , ReducedChunk (..)
    , valueChunk
    , valueFrame
    , UUID (..)
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Eq, Generic, NFData, Show)

data Op = Op
    { opType     :: {-# UNPACK #-} UUID
    , opObject   :: {-# UNPACK #-} UUID
    , opEvent    :: {-# UNPACK #-} UUID
    , opLocation :: {-# UNPACK #-} UUID
    , opPayload  ::                [Atom]
    }
    deriving (Eq, Generic, NFData, Show)

data ReducedChunk = ReducedChunk
    { chunkHeader :: Op
    , chunkBody   :: [Op]
    }
    deriving (Eq, Generic, NFData, Show)

data Chunk = Raw Op | Value ReducedChunk | Query ReducedChunk
    deriving (Eq, Generic, NFData, Show)

type Frame = [Chunk]

data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

valueChunk :: Op -> [Op] -> Chunk
valueChunk chunkHeader chunkBody = Value ReducedChunk{chunkHeader, chunkBody}

valueFrame :: Op -> [Op] -> Frame
valueFrame chunkHeader chunkBody = [valueChunk chunkHeader chunkBody]
