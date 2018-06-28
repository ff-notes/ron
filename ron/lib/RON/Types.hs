{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RON.Types
    ( Frame
    , Op (..)
    , UUID (..)
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

-- data Atom = AString ByteString | AInteger Integer | AFloat _ | AUuid UUID
--     deriving (Eq, Show)

data Op = Op
    { typ      :: {-# UNPACK #-} !UUID
    , object   :: {-# UNPACK #-} !UUID
    , event    :: {-# UNPACK #-} !UUID
    , location :: {-# UNPACK #-} !UUID
    -- , payload :: ![Atom]
    }
    deriving (Eq, Generic, NFData, Show)

-- | Frame, uncompressed
type Frame = [Op]
