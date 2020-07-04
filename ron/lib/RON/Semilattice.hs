{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module RON.Semilattice (
    Semilattice (..),
    BoundedSemilattice,
) where

import           Prelude

import           Data.Semigroup (Max)
import           Data.Set (Set)

{- |
A semilattice.

It may be a join-semilattice, or meet-semilattice, it doesn't matter.

If it matters for you, use package @lattices@.

In addition to 'Semigroup', Semilattice defines these laws:

[commutativity]

    @x '<>' y == y '<>' x@

[idempotency]

    @x '<>' x == x@

[relation-operation equivalence]

    @x '≼' y == (x '<>' y == y)@
    @x '<>' y == minimum \z -> x '≼' z && y '≼' z@
-}
class Semigroup a => Semilattice a where

    -- | Semilattice relation.
    (≼) :: a -> a -> Bool
    default (≼) :: Eq a => a -> a -> Bool
    a ≼ b = a <> b == b

{- |
A bounded semilattice.

Bounded semilattice laws are already defined by 'Monoid' and 'Semilattice',
so we don't define an explicit class here.
-}
type BoundedSemilattice a = (Monoid a, Semilattice a)

-- instances for base types

instance Ord a => Semilattice (Max a)

instance Ord a => Semilattice (Set a)

instance Semilattice a => Semilattice (Maybe a) where
    Nothing ≼ _       = True
    _       ≼ Nothing = False
    Just a  ≼ Just b  = a ≼ b
