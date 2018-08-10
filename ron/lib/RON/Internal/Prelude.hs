module RON.Internal.Prelude
    ( module RON.Internal.Prelude
    , module X
    ) where

import           Control.Applicative as X
import           Control.Monad as X
import           Data.ByteString as X (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Function as X
import           Data.Functor as X
import           Data.Int as X (Int16, Int32, Int64, Int8)
import           Data.Maybe as X
import           Data.Semigroup as X ((<>))
import           Data.Word as X (Word16, Word32, Word64, Word8)

type ByteStringL = BSL.ByteString
