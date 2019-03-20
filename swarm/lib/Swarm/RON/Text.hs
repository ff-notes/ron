module Swarm.RON.Text (
    TextFrame (..),
) where

import           Data.Proxy (Proxy)
import           Foreign (ForeignPtr)

-- | Class @ron::TextFrame@
newtype TextFrame = TextFrame (ForeignPtr (Proxy TextFrame))
