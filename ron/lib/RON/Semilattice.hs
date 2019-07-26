{-# LANGUAGE ConstraintKinds #-}

module RON.Semilattice (
    Semilattice,
    BoundedSemilattice,
) where

import           Data.Maybe (Maybe)
import           Data.Monoid (Monoid)
import           Data.Ord (Ord)
import           Data.Semigroup (Max, Semigroup)
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
-}
class Semigroup a => Semilattice a

{- |
A bounded semilattice.

Bounded semilattice laws are already defined by 'Monoid' and 'Semilattice',
so we don't define an explicit class here.
-}
type BoundedSemilattice a = (Monoid a, Semilattice a)

-- instances for base types

instance Ord a => Semilattice (Max a)

instance Ord a => Semilattice (Set a)

instance Semilattice a => Semilattice (Maybe a)
