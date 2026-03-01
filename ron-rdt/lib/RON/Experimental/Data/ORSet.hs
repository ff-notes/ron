{-# LANGUAGE DisambiguateRecordFields #-}

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
import RON.Types (Op (Op), OpenFrame, UUID)
import RON.Types qualified
import RON.Types.Experimental (Patch (Patch), Ref (Ref))
import RON.Types.Experimental qualified

decode :: UUID -> OpenFrame -> ORSet a
decode objectId ops =
    ORSet $
        Map.fromListWith
            (maxOn fst)
            [ (itemId, (opId, payload))
            | Op{opId, refId, payload} <- ops
            , opId /= objectId
            , let
                itemId =
                    case payload of
                        [] -> refId -- tombstone
                        _ : _ -> opId -- add
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
    let alivePayloads = [payload | (_opId, payload@(_ : _)) <- Map.elems items]
    traverse fromAtoms alivePayloads

empty :: ORSet a
empty = ORSet Map.empty
