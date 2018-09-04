{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), mkStateChunk)
import           RON.Types (Op' (..), UUID)

lww :: Op' -> Op' -> Op'
lww = maxOn opEvent

-- | Key is 'opRef', value is the original op
newtype LwwPerField = LwwPerField (Map UUID Op')
    deriving (Eq, Monoid, Show)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    type OpType LwwPerField = "lww"

    stateFromChunk ops =
        LwwPerField $ Map.fromListWith lww [(opRef op, op) | op <- ops]

    stateToChunk (LwwPerField fields) = mkStateChunk $ Map.elems fields
