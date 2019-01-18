{-# LANGUAGE LambdaCase #-}

module Prelude (module X, lastDef) where

import           Control.Applicative as X (Applicative, pure, (*>), (<*), (<*>))
import           Control.Monad as X (Monad, (=<<), (>>=))
import           Control.Monad.Fail as X (MonadFail, fail)
import           Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import           Data.Char as X (Char)
import           Data.Either as X (Either (Left, Right))
import           Data.Eq as X (Eq, (/=), (==))
import           Data.Foldable as X (Foldable, foldMap, null)
import           Data.Function as X (const, flip, ($), (.))
import           Data.Functor as X (Functor, fmap, (<$>))
import           Data.Int as X (Int)
import           Data.List as X (map, repeat, replicate, span, splitAt, take,
                                 takeWhile, unlines, unwords, (++))
import           Data.List (last)
import           Data.Maybe as X (Maybe (Just, Nothing), maybe)
import           Data.Monoid as X (Monoid, mempty)
import           Data.Ord as X (Ord, Ordering (EQ, GT, LT), compare, max, (<),
                                (<=), (>), (>=))
import           Data.Semigroup as X ((<>))
import           Data.String as X (String)
import           Data.Traversable as X (sequence, sequenceA, traverse)
import           Data.Tuple as X (fst, snd)
import           Data.Word as X (Word)
import           GHC.Enum as X (Bounded, Enum, fromEnum, maxBound, minBound,
                                pred, succ, toEnum)
import           GHC.Err as X (error, undefined)
import           GHC.Exts as X (Double)
import           GHC.Integer as X (Integer)
import           GHC.Num as X (Num, negate, subtract, (*), (+), (-))
import           GHC.Real as X (Integral, fromIntegral, mod, realToFrac, round)
import           GHC.Stack as X (HasCallStack)
import           System.IO as X (FilePath, IO)
import           Text.Show as X (Show, show)

lastDef :: a -> [a] -> a
lastDef def = \case
    [] -> def
    xs -> last xs
