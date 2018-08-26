{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.VersionVector
    ( VersionVector
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (OpType, Patch, Reducible, stateFromChunk,
                                    stateToChunk)
import           RON.Types (ROp (ROp), UUID (UUID), ropEvent)

type Origin = Word64

ropTime :: ROp -> Word64
ropTime ROp{ropEvent = UUID time _} = time

ropOrigin :: ROp -> Word64
ropOrigin ROp{ropEvent = UUID _ origin} = origin

latter :: ROp -> ROp -> ROp
latter = maxOn ropTime

newtype VersionVector = VersionVector (Map Origin ROp)

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

instance Reducible VersionVector where
    type OpType VersionVector = "vv"
    type Patch  VersionVector = VersionVector

    stateToChunk (VersionVector vv) = Map.elems vv

    stateFromChunk rops = VersionVector $
        Map.fromListWith latter [(ropOrigin rop, rop) | rop <- rops]
