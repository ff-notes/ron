{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Experimental.Data.ORMap (
    add_,
    lookupLww,
    lookupLwwThrow,
    lookupLwwDecode,
    lookupLwwDecodeThrow,
    lookupSet,
    remove,
    update,
) where

import RON.Prelude

import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import RON.Error (MonadE, liftMaybe)
import RON.Event (ReplicaClock)
import RON.Experimental.Data (AsAtom, AsAtoms, fromAtoms, toAtom)
import RON.Experimental.Data.ORSet qualified as ORSet
import RON.Experimental.Data.ORSet.Type (ORMap, ORSet (ORSet))
import RON.Store.Class (MonadStore, loadWholeObjectLog)
import RON.Text.Serialize (serializeAtom)
import RON.Types (Payload)
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
    for_ (Map.assocs items) \(itemId, payload) ->
        case payload of
            k : _
                | k == toAtom key -> ORSet.remove objectRef (Ref @(k, v) itemId)
            _ -> pure ()

update ::
    (AsAtom k, AsAtoms v, MonadStore m, ReplicaClock m) =>
    Ref (ORMap k v) -> k -> v -> m ()
update objectRef (key :: k) (value :: v) = do
    remove objectRef key
    add_ objectRef key value

lookupLww :: (AsAtom k) => k -> ORMap k v -> Maybe Payload
lookupLww key (ORSet s) =
    snd
        <$> maximumMayOn
            fst
            [(item, value) | (item, k : value) <- Map.assocs s, k == toAtom key]

-- | Like 'lookupLww' but also decode payload.
lookupLwwDecode ::
    (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m (Maybe v)
lookupLwwDecode key = traverse fromAtoms . lookupLww key

lookupLwwThrow :: (AsAtom k, MonadE m) => k -> ORMap k v -> m Payload
lookupLwwThrow key obj =
    liftMaybe ("key " <> showAtom key <> " must present") $ lookupLww key obj
  where
    showAtom = TextL.toStrict . TextL.decodeUtf8 . serializeAtom . toAtom

-- | Like 'lookupLwwDecode' but assert that key exists.
lookupLwwDecodeThrow :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m v
lookupLwwDecodeThrow key = lookupLwwThrow key >=> fromAtoms

lookupSet :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m [v]
lookupSet key (ORSet s) =
    traverse fromAtoms [value | k : value <- Map.elems s, k == toAtom key]
