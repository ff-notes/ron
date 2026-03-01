{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module RON.Experimental.Data.ORSet (
    ORSet,
    ORMap,
    add,
    add_,
    decode,
    empty,
    getDecode,
    remove,
) where

import RON.Prelude

import Data.Map.Strict qualified as Map
import RON.Error (MonadE)
import RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import RON.Experimental.Data (AsAtoms, fromAtoms, toAtoms)
import RON.Experimental.Data.ORSet.Type (ORMap, ORSet (ORSet))
import RON.Store.Class (MonadStore, appendPatch, loadWholeObjectLog)
import RON.Types (Op (Op), OpenFrame, Payload, UUID)
import RON.Types qualified
import RON.Types.Experimental (Patch (Patch), Ref (Ref))
import RON.Types.Experimental qualified

preferTombstone :: Payload -> Payload -> Payload
preferTombstone xs = \case [] -> []; _ -> xs

itemId :: Op -> UUID
itemId Op{opId, refId, payload} =
    case payload of
        [] -> refId -- tombstone
        _ : _ -> opId -- add

decode :: UUID -> OpenFrame -> ORSet a
decode objectId ops =
    ORSet
        . Map.filter (not . null)
        $ Map.fromListWith
            preferTombstone
            [ (itemId op, payload)
            | op@Op{opId, payload} <- ops
            , opId /= objectId
            ]

-- | Add value to the set. Return the reference to the set item.
add ::
    (AsAtoms item, MonadStore m, ReplicaClock m) =>
    Ref (ORSet item) -> item -> m UUID
add (Ref object) value = do
    advanceToUuid object
    opId <- getEventUuid
    appendPatch
        Patch
            { object
            , log = Op{opId, refId = object, payload = toAtoms value} :| []
            }
    pure opId

{- |
    Add value to the set or map.

    @add_ :: Ref (ORSet a)   -> a      -> m ()@
    @add_ :: Ref (ORMap k v) -> (k, v) -> m ()@
-}
add_ ::
    (AsAtoms item, MonadStore m, ReplicaClock m) =>
    Ref (ORSet item) -> item -> m ()
add_ ref = void . add ref

remove :: (MonadStore m, ReplicaClock m) => Ref (ORSet a) -> Ref a -> m ()
remove (Ref object) (Ref refId) = do
    advanceToUuid refId
    opId <- getEventUuid
    appendPatch Patch{object, log = Op{opId, refId, payload = []} :| []}

-- | Get items from database and decode
getDecode :: (AsAtoms a, MonadE m, MonadStore m) => Ref (ORSet a) -> m [a]
getDecode (Ref object) = do
    -- TODO loadObjectLog object (PayloadPrefix pre)
    ops <- loadWholeObjectLog object mempty
    let ORSet items = decode object ops
    let alivePayloads = Map.elems items
    traverse fromAtoms alivePayloads

empty :: ORSet a
empty = ORSet Map.empty
