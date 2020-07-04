{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Version Vector
module RON.Data.VersionVector (
    VersionVector,
    lookup,
    (·≼),
) where

import           RON.Prelude hiding (lookup)

import           Data.Hashable (hashWithSalt)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal (Reducible, Rep, Replicated (encoding),
                                    ReplicatedAsObject, getObjectState,
                                    newObject, objectEncoding, readObject,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Event (ReplicaId, decodeEvent, getEventUuid, replicaId)
import           RON.Semilattice (Semilattice, (≼))
import           RON.Types (ObjectRef (ObjectRef), Op (Op, opId), UUID (UUID), WireStateChunk (WireStateChunk, stateBody, stateType))
import qualified RON.UUID as UUID

opTime :: Op -> Word64
opTime Op{opId = UUID time _} = time

opReplica :: Op -> ReplicaId
opReplica Op{opId} = replicaId $ decodeEvent opId

latter :: Op -> Op -> Op
latter = maxOn opTime

-- | Version Vector type. May be used both in typed and untyped contexts.
newtype VersionVector = VersionVector (Map ReplicaId Op)
    deriving (Eq, Show)

instance Hashable VersionVector where
    hashWithSalt s (VersionVector vv) = hashWithSalt s $ Map.assocs vv

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

-- | Laws:
-- 1. Idempotent because 'Map.unionWith' is idempotent.
-- 2. Commutative because 'latter' is commutative.
instance Semilattice VersionVector where
  VersionVector a ≼ VersionVector b =
    case leftJoin (Map.assocs a) (Map.assocs b) of
      Nothing     -> False
      Just joined -> and [opTime opA <= opTime opB | (opA, opB) <- joined]
    where
      -- leftJoin :: Ord k => [(k, v1)] -> [(k, v2)] -> Maybe [(v1, v2)]
      leftJoin []             _              = Just []
      leftJoin (_:_)          []             = Nothing
      leftJoin ((xk, xv):xs') ((yk, yv):ys') =
        case compare xk yk of
          LT -> Nothing                           -- x must present but doesn't
          EQ -> ((xv, yv) :) <$> leftJoin xs' ys' -- intersection
          GT -> leftJoin xs' ys'                  -- skip extra y

instance Reducible VersionVector where
    reducibleOpType = vvType

    stateFromChunk ops =
        VersionVector $ Map.fromListWith latter [(opReplica op, op) | op <- ops]

    stateToChunk (VersionVector vv) = Map.elems vv

wireStateChunk :: [Op] -> WireStateChunk
wireStateChunk stateBody = WireStateChunk{stateType = vvType, stateBody}

-- | Name-UUID to use as Version Vector type marker.
vvType :: UUID
vvType = $(UUID.liftName "vv")

instance Replicated VersionVector where
    encoding = objectEncoding

instance ReplicatedAsObject VersionVector where
    type Rep VersionVector = VersionVector

    newObject (VersionVector vv) = do
        oid <- getEventUuid
        let ops = Map.elems vv
        modify' $ Map.insert oid $ wireStateChunk ops
        pure $ ObjectRef oid

    readObject = getObjectState

(·≼) :: UUID -> VersionVector -> Bool
uuid@(UUID time _) ·≼ VersionVector vv =
  maybe False ((time <=) . opTime) $
  Map.lookup (replicaId $ decodeEvent uuid) vv

lookup :: ReplicaId -> VersionVector -> Maybe Op
lookup replica (VersionVector vv) = Map.lookup replica vv
