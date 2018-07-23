{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Data.RGA where

import           Data.Algorithm.Diff (Diff (Both, First, Second),
                                      getGroupedDiffBy)
import           Data.Function (on)
import           Data.List (genericLength)
import           Data.Maybe (fromJust, maybeToList)
import           Data.Traversable (for)

import           RON.Event (Clock, Event, encodeEvent, getEvents)
import           RON.Typed (AsAtom, Replicated, toAtom, toReducedOps)
import           RON.Types (Op (..), UUID)
import qualified RON.UUID as UUID

-- | We need to compare ids by uuidValue timestamp first.
-- 'UUID's are sorted by variant first,
-- which is part of replicaId in case of 'Event'.
type VertexId = Event

newtype RGA a = RGA [(VertexId, Maybe a)]
    deriving (Eq, Show)

type RgaString = RGA Char

merge :: Eq a => RGA a -> RGA a -> RGA a
merge (RGA vertices1) (RGA vertices2) =
    RGA $ mergeVertexLists vertices1 vertices2
  where
    mergeVertexLists []                 vs2                = vs2
    mergeVertexLists vs1                []                 = vs1
    mergeVertexLists (v1@(id1, a1):vs1) (v2@(id2, a2):vs2) =
        case compare id1 id2 of
            LT -> v2 : mergeVertexLists (v1 : vs1) vs2
            GT -> v1 : mergeVertexLists vs1 (v2 : vs2)
            EQ -> (id1, mergeAtoms a1 a2) : mergeVertexLists vs1 vs2

    -- priority of deletion
    mergeAtoms Nothing   _         = Nothing
    mergeAtoms _         Nothing   = Nothing
    mergeAtoms (Just a1) (Just a2)
        | a1 == a2  = Just a1
        | otherwise = Nothing -- error: contradiction

instance Eq a => Semigroup (RGA a) where
    (<>) = merge

-- instance (Eq a, AsEmpty a) => Semilattice (RGA a)

-- Why not?
instance Eq a => Monoid (RGA a) where
    mempty = RGA []
    mappend = (<>)

toList :: RGA a -> [a]
toList (RGA rga) = [a | (_, Just a) <- rga]

toString :: RgaString -> String
toString = toList

fromList :: Clock m => [a] -> m (RGA a)
fromList = fmap RGA . fromList'

fromList' :: Clock m => [a] -> m [(VertexId, Maybe a)]
fromList' xs = do
    vids <- getEvents $ genericLength xs
    pure $ zip vids $ map Just xs

fromString :: Clock m => String -> m RgaString
fromString = fromList

-- | Replace content with specified,
-- applying changed found by the diff algorithm
edit :: (Eq a, Clock m) => [a] -> RGA a -> m (RGA a)
edit newList (RGA oldRga) =
    fmap (RGA . concat) . for diff $ \case
        First removed -> pure [(vid, Nothing) | (vid, _) <- removed]
        Both v _      -> pure v
        Second added  -> fromList' [a | (_, Just a) <- added]
  where
    newList' = [(undefined, Just a) | a <- newList]
    diff     = getGroupedDiffBy ((==) `on` snd) oldRga newList'

newtype RgaText = RgaText RgaString
    deriving (Replicated)

rgaType :: UUID
rgaType = fromJust $ UUID.mkName "rga"

instance AsAtom a => Replicated (RGA a) where
    toReducedOps opObject (RGA rga) = for rga mkStateOp
      where
        mkStateOp (vid, a) = do
            opEvent <-
                maybe (fail "VertexId is a bad Event") pure $ encodeEvent vid
            pure Op{..}
          where
            opPayload   = toAtom <$> maybeToList a
            opType      = rgaType
            opLocation  = UUID.zero
