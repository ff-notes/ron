{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Version Vector
module RON.Data.VersionVector (
    VersionVector,
    VV (..),
    lookup,
    mkVV,
    serializeVV,
    (·≻),
    (·≼),
) where

import RON.Prelude hiding (lookup)

import Data.Hashable (hashWithSalt)
import Data.Map.Strict qualified as Map

import RON.Data.Internal (
    Reducible,
    Rep,
    Replicated (encoding),
    ReplicatedAsObject,
    getObjectState,
    newObject,
    objectEncoding,
    readObject,
    reducibleOpType,
    stateFromChunk,
    stateToChunk,
 )
import RON.Event (
    Event (..),
    Replica,
    Time,
    encodeEvent,
    getEventUuid,
    replica,
    unsafeDecodeEvent,
 )
import RON.Semilattice (Semilattice, (≼))
import RON.Text.Serialize (serializePayload)
import RON.Types (
    Atom (AUuid),
    ObjectRef (ObjectRef),
    Op (Op, opId),
    UUID (UUID),
    WireStateChunk (WireStateChunk, stateBody, stateType),
 )
import RON.UUID qualified as UUID

opTime :: Op -> Word64
opTime Op{opId = UUID time _} = time

-- | Assume opId is event
opReplica :: Op -> Replica
opReplica Op{opId} = replica $ unsafeDecodeEvent opId

latter :: Op -> Op -> Op
latter = maxOn opTime

-- | Version Vector type. May be used both in typed and untyped contexts.
newtype VersionVector = VersionVector (Map Replica Op)
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

lookup :: Replica -> VersionVector -> Maybe Op
lookup replica (VersionVector vv) = Map.lookup replica vv

-- | Simplified version vector
newtype VV = VV (Map Replica Time)
  deriving (Show)

instance Semigroup VV where
  VV a <> VV b = VV $ Map.unionWith max a b

instance Monoid VV where
  mempty = VV Map.empty

-- | Assuming all UUIDs are events.
mkVV :: [UUID] -> VV
mkVV uuids =
  VV $
    Map.fromList
      [ (replica, time)
      | uuid <- uuids, let Event{replica, time} = unsafeDecodeEvent uuid
      ]

unVV :: VV -> [UUID]
unVV (VV vv) =
  [encodeEvent Event{replica, time} | (replica, time) <- Map.assocs vv]

(·≻) :: UUID -> VV -> Bool
uuid ·≻ VV vv =
  maybe True (time >) $ Map.lookup replica vv
  where
    Event{replica, time} = unsafeDecodeEvent uuid

(·≼) :: UUID -> VV -> Bool
uuid ·≼ VV vv =
  maybe False (time <=) $ Map.lookup replica vv
  where
    Event{replica, time} = unsafeDecodeEvent uuid

serializeVV :: VV -> ByteStringL
serializeVV = serializePayload . map AUuid . unVV
