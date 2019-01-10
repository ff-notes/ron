module RON.Internal.Prelude
    ( module RON.Internal.Prelude
    , module X
    ) where

import           Control.Applicative as X
import           Control.Monad as X
import           Control.Monad.Except as X (throwError)
import           Data.ByteString as X (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce as X
import           Data.Either as X
import           Data.Foldable as X
import           Data.Function as X
import           Data.Functor as X
import           Data.Functor.Identity as X
import           Data.Hashable as X (Hashable, hashWithSalt)
import           Data.HashMap.Strict as X (HashMap)
import           Data.HashSet as X (HashSet)
import           Data.Int as X (Int16, Int32, Int64, Int8)
import           Data.List as X (foldl', partition, sort, sortBy, sortOn)
import           Data.List.NonEmpty as X (NonEmpty ((:|)), nonEmpty)
import           Data.Map.Strict as X (Map)
import           Data.Maybe as X
import           Data.Proxy as X
import           Data.Semigroup as X (sconcat, (<>))
import           Data.Set as X (Set)
import           Data.Text as X (Text)
import           Data.Traversable as X
import           Data.Tuple.Extra as X
import           Data.Vector as X (Vector)
import           Data.Word as X (Word16, Word32, Word64, Word8)
import           GHC.Generics as X
import           GHC.TypeLits as X
import           Safe.Foldable as X

type ByteStringL = BSL.ByteString

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x < f y then y else x

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y = if f x < f y then x else y

newtype MaxOnFst a b = MaxOnFst (a, b)

instance Ord a => Semigroup (MaxOnFst a b) where
    mof1@(MaxOnFst (a1, _)) <> mof2@(MaxOnFst (a2, _))
        | a1 < a2   = mof2
        | otherwise = mof1
