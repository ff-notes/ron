{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RON.Types
    ( Atom (..)
    , Frame
    , Op (..)
    , UUID (..)
    ) where

import           Internal.Prelude

import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

newtype Atom = AInteger Int64
    deriving (Eq, Generic, NFData, Show)

data Op = Op
    { opType     :: {-# UNPACK #-} !UUID
    , opObject   :: {-# UNPACK #-} !UUID
    , opEvent    :: {-# UNPACK #-} !UUID
    , opLocation :: {-# UNPACK #-} !UUID
    , opPayload  :: {-# UNPACK #-} ![Atom]
    }
    deriving (Eq, Generic, NFData, Show)

-- | Frame, uncompressed
type Frame = [Op]
