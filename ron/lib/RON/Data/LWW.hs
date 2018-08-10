{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.LWW where

import           Data.Maybe (fromJust)

import           RON.Event (EpochEvent, encodeEvent, fromEpochEvent, getEvent)
import           RON.Typed (AsAtom, Replicated, View, initialize, toAtom,
                            toStateChunk, toStateOps, view)
import           RON.Types (Op (..), ReducedChunk (..), UUID)
import qualified RON.UUID as UUID

data LWW a = LWW
    { time  :: !EpochEvent
    , value :: !a
    }
    deriving (Eq, Show)

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

listSingleton :: a -> [a]
listSingleton x = [x]
