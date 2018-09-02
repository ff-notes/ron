{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.VersionVector
    ( VersionVector
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), mkReducedState)
import           RON.Types (Op' (..), UUID (UUID))

type Origin = Word64

ropTime :: Op' -> Word64
ropTime Op'{opEvent = UUID time _} = time

ropOrigin :: Op' -> Word64
ropOrigin Op'{opEvent = UUID _ origin} = origin

latter :: Op' -> Op' -> Op'
latter = maxOn ropTime

newtype VersionVector = VersionVector (Map Origin Op')
    deriving (Eq)

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

instance Reducible VersionVector where
    type OpType VersionVector = "vv"

    fromRawOp = undefined

    fromChunk _ ops =
        VersionVector $ Map.fromListWith latter [(ropOrigin op, op) | op <- ops]

    toChunks (VersionVector vv) = mkReducedState $ Map.elems vv
