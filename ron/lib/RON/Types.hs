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
    , stateChunk
    , stateFrame
    , UUID (..)
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

data Atom = AInteger Int64 | AString Text | AUuid UUID
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

data Chunk = Raw Op | State ReducedChunk | Query ReducedChunk
    deriving (Eq, Generic, NFData, Show)

type Frame = [Chunk]

data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

stateChunk :: Op -> [Op] -> Chunk
stateChunk chunkHeader chunkBody = State ReducedChunk{chunkHeader, chunkBody}

stateFrame :: Op -> [Op] -> Frame
stateFrame chunkHeader chunkBody = [stateChunk chunkHeader chunkBody]
