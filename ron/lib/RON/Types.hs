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
    , reducedChunk
    , ReducedFrame
    , reducedFrame
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
    { chunkHeader   :: Op
    , chunkIsQuery  :: Bool
    , chunkBody     :: [Op]
    }
    deriving (Eq, Generic, NFData, Show)

data Chunk = Raw Op | Reduced ReducedChunk
    deriving (Eq, Generic, NFData, Show)

type Frame = [Chunk]

type ReducedFrame = ReducedChunk

data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

reducedChunk :: Op -> [Op] -> Chunk
reducedChunk chunkHeader chunkBody =
    Reduced ReducedChunk{chunkHeader, chunkIsQuery = False, chunkBody}

reducedFrame :: Op -> [Op] -> Frame
reducedFrame chunkHeader chunkBody = [reducedChunk chunkHeader chunkBody]
