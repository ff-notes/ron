{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (OpType, Patch, Reducible, stateFromChunk,
                                    stateToChunk)
import           RON.Types (ROp, UUID, ropEvent, ropLocation)

lww :: ROp -> ROp -> ROp
lww = maxOn ropEvent

newtype LwwPerField = LwwPerField (Map UUID ROp)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Monoid LwwPerField where
    mempty = LwwPerField mempty

instance Reducible LwwPerField where
    type OpType LwwPerField = "lww"
    type Patch  LwwPerField = LwwPerField

    stateFromChunk rops = LwwPerField $
        Map.fromListWith lww [(ropLocation rop, rop) | rop <- rops]

    stateToChunk (LwwPerField fields) = Map.elems fields
