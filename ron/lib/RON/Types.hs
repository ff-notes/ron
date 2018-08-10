{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module RON.Types
    ( Atom (..)
    , Chunk (..)
    , Frame
    , Op (..)
    , OpTerm (..)
    , ReducedChunk (..)
    , ReducedFrame
    , UUID (..)
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

data Atom = AUuid UUID | AInteger Int64
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
