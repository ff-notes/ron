{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.RGA where

import           Data.Algorithm.Diff (Diff (Both, First, Second),
                                      getGroupedDiffBy)
import           Data.Coerce (coerce)
import           Data.Function (on)
import           Data.Maybe (fromJust, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)

import           RON.Event (Clock, EpochEvent, decodeEvent, encodeEvent,
                            fromEpochEvent, getEvents, toEpochEvent)
import           RON.Internal.Word (leastSignificant60)
import           RON.Typed (AsAtom, Replicated, View, fromAtom, fromStateChunk,
                            fromStateOps, initialize, toAtom, toStateChunk,
                            toStateOps, view)
import           RON.Types (Op (..), ReducedChunk (..), UUID)
import qualified RON.UUID as UUID

-- | 'EpochEvent' because we need comparable events
type VertexId = EpochEvent

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
    vids <- getEvents $ leastSignificant60 $ length xs
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

rgaType :: UUID
rgaType = fromJust $ UUID.mkName "rga"

instance AsAtom a => Replicated (RGA a) where
    type View (RGA a) = [a]
    initialize = fromList
    view = toList

    toStateOps opObject (RGA rga) = for rga $ \(vid, a) -> do
        opEvent <-
            maybe (fail "VertexId is a bad Event") pure $
            encodeEvent $ fromEpochEvent vid
        pure Op
            { opObject
            , opEvent
            , opLocation = UUID.zero
            , opType     = rgaType
            , opPayload  = toAtom <$> maybeToList a
            }

    toStateChunk this rga = do
        chunkBody <- toStateOps this rga
        pure ReducedChunk
            { chunkHeader  = Op
                { opObject   = this
                , opEvent    = UUID.zero
                , opLocation = UUID.zero
                , opType     = rgaType
                , opPayload  = []
                }
            , chunkBody
            }

    fromStateOps   _ ownOps _ = rgaFromStateOps ownOps
    fromStateChunk _ ownOps _ = rgaFromStateOps ownOps

rgaFromStateOps :: AsAtom a => [Op] -> Either String (RGA a)
rgaFromStateOps ownOps =
    fmap RGA . for ownOps $ \Op{opEvent, opPayload} -> do
        event <- maybe (Left "Bad opEvent") Right $ decodeEvent  opEvent
        vid   <- maybe (Left "Bad event")   Right $ toEpochEvent event
        case opPayload of
            []  -> pure (vid, Nothing)
            [a] -> do
                x <- maybe (Left "Bad atom") Right $ fromAtom a
                pure (vid, Just x)
            _   -> Left "Bad opPayload"

newtype RgaText = RgaText RgaString
    deriving (Eq, Show)

instance Replicated RgaText where
    type View RgaText = Text
    initialize = fmap RgaText . initialize . Text.unpack
    view = Text.pack . coerce (view @RgaString)
    toStateOps     = coerce $ toStateOps     @RgaString
    toStateChunk   = coerce $ toStateChunk   @RgaString
    fromStateOps   = coerce $ fromStateOps   @RgaString
    fromStateChunk = coerce $ fromStateChunk @RgaString
