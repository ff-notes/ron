{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Experimental.Data.ORSet (
  ORSet,
  ORMap,
  add,
  add_,
  decode,
  empty,
  getDecode,
  lookupLww,
  lookupLwwThrow,
  lookupLwwDecode,
  lookupLwwDecodeThrow,
  lookupSet,
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL

import           Data.List (stripPrefix)
import           RON.Error (MonadE, liftMaybe)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Experimental.Data (AsAtom, AsAtoms, fromAtoms, toAtom,
                                        toAtoms)
import           RON.Store.Class (MonadStore, appendPatch, loadWholeObjectLog)
import           RON.Text.Serialize (serializeAtom)
import           RON.Types (Op (..), OpenFrame, Payload, UUID)
import           RON.Types.Experimental (Patch (..), Ref (..))

import           RON.Experimental.Data.ORSet.Type (ORMap, ORSet (..))

decode :: Applicative f => UUID -> OpenFrame -> f (ORSet a)
decode objectId ops =
  pure $
  ORSet $
  Map.fromListWith
    (maxOn fst)
    [ (itemId, (opId, payload))
    | Op{opId, refId, payload} <- ops
    , opId /= objectId
    , let
      itemId =
        case payload of
          []  -> refId  -- tombstone
          _:_ -> opId   -- add
    ]

-- | Add value to the set. Return the reference to the set item.
add ::
  (AsAtoms item, MonadStore m, ReplicaClock m) =>
  Ref (ORSet item) -> item -> m UUID
add (Ref object path) value = do
  advanceToUuid object
  opId <- getEventUuid
  appendPatch
    Patch
      { object
      , log = Op{opId, refId = object, payload = path ++ toAtoms value} :| []
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

-- | Get items from database and decode
getDecode :: (AsAtoms a, MonadStore m, MonadE m) => Ref (ORSet a) -> m [a]
getDecode (Ref object pre) = do
  -- TODO loadObjectLog object (PayloadPrefix pre)
  ops <- loadWholeObjectLog object mempty
  let alivePayloads =
        filter (not . null) $
        map snd $
        toList $
        Map.fromListWith
          (maxOn fst)
          [ (itemId, (opId, payload'))
          | Op{opId, refId, payload} <- ops
          , (itemId, payload') <-
              case payload of
                []                            -> pure (refId, payload)
                (stripPrefix pre -> Just suf) -> pure (opId, suf)
                _                             -> []
          ]
  traverse fromAtoms alivePayloads

lookupLww :: AsAtom k => k -> ORMap k v -> Maybe Payload
lookupLww key (ORSet s) =
  snd <$>
  maximumMayOn
    fst
    [(item, value) | (item, k : value) <- Map.elems s, k == toAtom key]

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

empty :: ORSet a
empty = ORSet Map.empty

lookupSet :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m [v]
lookupSet key (ORSet s) =
  traverse
    fromAtoms
    [value | (_item, k : value) <- Map.elems s, k == toAtom key]
