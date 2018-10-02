{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Data.VersionVector
    ( VersionVector (..)
    ) where

import           RON.Internal.Prelude

import           Control.Monad.Writer.Strict (lift, tell)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Event (getEventUuid)
import           RON.Types (Op (..), StateChunk (..), UUID (UUID))
import qualified RON.UUID as UUID

type Origin = Word64

opTime :: Op -> Word64
opTime Op{opEvent = UUID time _} = time

opOrigin :: Op -> Word64
opOrigin Op{opEvent = UUID _ origin} = origin

latter :: Op -> Op -> Op
latter = maxOn opTime

newtype VersionVector = VersionVector (Map Origin Op)
    deriving (Eq, Show)

instance Hashable VersionVector where
    hashWithSalt s (VersionVector vv) = hashWithSalt s $ Map.assocs vv

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

instance Reducible VersionVector where
    reducibleOpType = vvType

    stateFromChunk ops =
        VersionVector $ Map.fromListWith latter [(opOrigin op, op) | op <- ops]

    stateToChunk (VersionVector vv) = mkStateChunk $ Map.elems vv

vvType :: UUID
vvType = fromJust $ UUID.mkName "vv"

instance Replicated VersionVector where
    encoding = objectEncoding

instance ReplicatedAsObject VersionVector where
    objectOpType = vvType

    newObject (VersionVector vv) = collectFrame $ do
        oid <- lift getEventUuid
        let ops = Map.elems vv
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (vvType, oid) $ StateChunk version ops
        pure oid

    getObject obj = do
        StateChunk{..} <- getObjectStateChunk obj
        pure $ stateFromChunk stateBody
