module RON.Types
    ( Frame
    , Op (..)
    , UUID (..)
    ) where

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
    deriving (Eq, Show)

-- | Frame, uncompressed
type Frame = [Op]
