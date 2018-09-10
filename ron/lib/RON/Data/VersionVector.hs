{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.VersionVector
    ( VersionVector (..)
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), mkStateChunk)
import           RON.Types (Op' (..), UUID (UUID))

type Origin = Word64

opTime :: Op' -> Word64
opTime Op'{opEvent = UUID time _} = time

opOrigin :: Op' -> Word64
opOrigin Op'{opEvent = UUID _ origin} = origin

latter :: Op' -> Op' -> Op'
latter = maxOn opTime

newtype VersionVector = VersionVector (Map Origin Op')
    deriving (Eq, Show)

instance Hashable VersionVector where
    hashWithSalt s (VersionVector vv) = hashWithSalt s $ Map.assocs vv

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

instance Reducible VersionVector where
    type OpType VersionVector = "vv"

    stateFromChunk ops =
        VersionVector $ Map.fromListWith latter [(opOrigin op, op) | op <- ops]

    stateToChunk (VersionVector vv) = mkStateChunk $ Map.elems vv
