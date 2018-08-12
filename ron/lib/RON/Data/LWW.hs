{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (fromJust)
import           Data.Semigroup (Semigroup, sconcat)

import           RON.Event (EpochEvent, decodeEvent, encodeEvent,
                            fromEpochEvent, getEvent, toEpochEvent)
import           RON.Typed (AsAtom, Replicated, View, fromAtom, fromStateChunk,
                            fromStateOps, initialize, toAtom, toStateChunk,
                            toStateOps, view)
import           RON.Types (Op (..), ReducedChunk (..), UUID)
import qualified RON.UUID as UUID

data LWW a = LWW
    { time  :: !EpochEvent
    , value :: !a
    }
    deriving (Eq, Show)

-- | Merge by choosing more recent timestamp.
instance Semigroup (LWW a) where
    x <> y
        | time x < time y = y
        | otherwise       = x

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

instance AsAtom a => Replicated (LWW a) where
    type View (LWW a) = a
    view = value

    initialize value = do
        time <- getEvent
        pure LWW{time, value}

    toStateOps this lww = listSingleton <$> toOp this lww

    toStateChunk this lww = do
        chunkHeader <- toOp this lww
        pure ReducedChunk{chunkHeader, chunkBody = []}

    fromStateOps _ ownOps _ = case ownOps of
        []     -> Left "Empty state"
        op:ops -> fmap sconcat . traverse fromOp $ op :| ops

    fromStateChunk op _ _ = fromOp op

toOp :: (Monad m, AsAtom a) => UUID -> LWW a -> m Op
toOp this LWW{time, value} = do
    opEvent <-
        maybe (fail "LWW.time is a bad Event") pure $
        encodeEvent $ fromEpochEvent time
    pure Op
        { opType     = lwwType
        , opObject   = this
        , opEvent
        , opLocation = UUID.zero
        , opPayload  = [toAtom value]
        }

fromOp :: AsAtom a => Op -> Either String (LWW a)
fromOp Op{opEvent, opPayload} = do
    event <- maybe (Left "Bad opEvent") Right $ decodeEvent  opEvent
    time  <- maybe (Left "Bad event")   Right $ toEpochEvent event
    case opPayload of
        [a] -> do
            value <- maybe (Left "Bad atom") Right $ fromAtom a
            pure LWW{time, value}
        _   -> Left "Bad opPayload"

listSingleton :: a -> [a]
listSingleton x = [x]
