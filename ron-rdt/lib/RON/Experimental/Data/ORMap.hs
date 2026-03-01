{-# LANGUAGE BlockArguments #-}

module RON.Experimental.Data.ORMap (add_, remove, update) where

import RON.Prelude

import RON.Event (ReplicaClock)
import RON.Experimental.Data (AsAtom, AsAtoms, toAtom)
import RON.Experimental.Data.ORSet qualified as ORSet
import RON.Experimental.Data.ORSet.Type (ORMap, ORSet (ORSet))
import RON.Store.Class (MonadStore, loadWholeObjectLog)
import RON.Types.Experimental (Ref (Ref))

add_ ::
    (AsAtom k, AsAtoms v, MonadStore m, ReplicaClock m) =>
    Ref (ORMap k v) -> k -> v -> m ()
add_ object k v = ORSet.add_ object (k, v)

remove ::
    (AsAtom k, MonadStore m, ReplicaClock m) => Ref (ORMap k v) -> k -> m ()
remove objectRef@(Ref object :: Ref (ORMap k v)) key = do
    ops <- loadWholeObjectLog object mempty
    let ORSet items = ORSet.decode object ops
    for_ items \(opId, payload) ->
        case payload of
            k : _ | k == toAtom key -> ORSet.remove objectRef (Ref @(k, v) opId)
            _ -> pure ()

update ::
    (AsAtom k, AsAtoms v, MonadStore m, ReplicaClock m) =>
    Ref (ORMap k v) -> k -> v -> m ()
update objectRef (key :: k) (value :: v) = do
    remove objectRef key
    add_ objectRef key value
