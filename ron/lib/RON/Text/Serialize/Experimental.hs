module RON.Text.Serialize.Experimental (serializePatch) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC

import           RON.Text.Serialize (serializeOp)
import           RON.Types (Op (..))

serializePatch :: [Op] -> ByteStringL
serializePatch = BSLC.unlines . map serializeOp
