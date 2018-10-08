{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module RON.Types
    ( Atom (..)
    , Object (..)
    , ObjectId
    , Op (..)
    , OpTerm (..)
    , RawOp (..)
    , RChunk (..)
    , StateChunk (..)
    , StateFrame
    , UUID (..)
    , valueChunk
    , valueFrame
    , WireChunk (..)
    , WireFrame
    ) where

import           RON.Internal.Prelude

import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Data, Eq, Generic, Hashable, NFData, Show)

data RawOp = RawOp
    { opType   :: UUID
    , opObject :: UUID
    , op       :: Op
    }
    deriving (Data, Eq, Generic, NFData)

data Op = Op
    { opEvent   :: UUID
    , opRef     :: UUID
    , opPayload :: [Atom]
    }
    deriving (Data, Eq, Generic, Hashable, NFData, Show)

instance Show RawOp where
    show RawOp{opType, opObject, op = Op{opEvent, opRef, opPayload}} =
        unwords
            [ "RawOp"
            , insert '*' $ show opType
            , insert '#' $ show opObject
            , insert '@' $ show opEvent
            , insert ':' $ show opRef
            , show opPayload
            ]
      where
        insert k = \case
            []   -> [k]
            c:cs -> c:k:cs

data RChunk = RChunk
    { rchunkHeader :: RawOp
    , rchunkBody   :: [Op]
    }
    deriving (Data, Eq, Generic, NFData, Show)

data WireChunk = Raw RawOp | Value RChunk | Query RChunk
    deriving (Data, Eq, Generic, NFData, Show)

type WireFrame = [WireChunk]

data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

valueChunk :: RawOp -> [Op] -> WireChunk
valueChunk rchunkHeader rchunkBody = Value RChunk{rchunkHeader, rchunkBody}

valueFrame :: RawOp -> [Op] -> WireFrame
valueFrame rchunkHeader rchunkBody = [valueChunk rchunkHeader rchunkBody]

-- | (type, object)
type ObjectId = (UUID, UUID)

data StateChunk = StateChunk
    { stateVersion :: UUID
    , stateBody    :: [Op]
    }
    deriving (Eq, Show)

type StateFrame = Map ObjectId StateChunk

data Object a = Object{objectId :: UUID, objectFrame :: StateFrame}
    deriving (Eq, Show)
