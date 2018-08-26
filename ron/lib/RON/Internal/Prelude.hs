module RON.Internal.Prelude
    ( module RON.Internal.Prelude
    , module X
    ) where

import           Control.Applicative as X
import           Control.Monad as X
import           Data.ByteString as X (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce as X
import           Data.Either as X
import           Data.Foldable as X
import           Data.Function as X
import           Data.Functor as X
import           Data.Int as X (Int16, Int32, Int64, Int8)
import           Data.List as X (foldl')
import           Data.List.NonEmpty as X (NonEmpty ((:|)))
import           Data.Map.Strict as X (Map)
import           Data.Maybe as X
import           Data.Proxy as X
import           Data.Semigroup as X (sconcat, (<>))
import           Data.Traversable as X
import           Data.Word as X (Word16, Word32, Word64, Word8)
import           Safe.Foldable as X

type ByteStringL = BSL.ByteString

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x < f y then y else x
