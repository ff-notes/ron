{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW
    ( LwwPerField
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible (..), mkReducedPatch,
                                    mkReducedState)
import           RON.Types (Op' (..), UUID)
import           RON.UUID (pattern Zero)

lww :: Op' -> Op' -> Op'
lww = maxOn opEvent

-- | Key is 'opRef', value is the original op
data LwwPerField = LwwPerField{lpfRef :: Maybe UUID, lpfFields :: Map UUID Op'}
    deriving (Eq)

instance Semigroup LwwPerField where
    LwwPerField ref1 fields1 <> LwwPerField ref2 fields2 =
        LwwPerField (min ref1 ref2) (Map.unionWith lww fields1 fields2)

instance Monoid LwwPerField where
    mempty = LwwPerField{lpfRef = Nothing, lpfFields = mempty}

instance Reducible LwwPerField where
    type OpType LwwPerField = "lww"

    fromRawOp op@Op'{opEvent, opRef} = LwwPerField
        {lpfRef = Just opEvent, lpfFields = Map.singleton opRef op}

    fromChunk ref ops = LwwPerField
        { lpfRef    = Just ref
        , lpfFields = Map.fromListWith lww [(opRef op, op) | op <- ops]
        }

    toChunks LwwPerField{lpfRef, lpfFields} = case fromMaybe Zero lpfRef of
        Zero -> mkReducedState     $ Map.elems lpfFields
        ref  -> mkReducedPatch ref $ Map.elems lpfFields

    sameState = (==) `on` lpfFields
