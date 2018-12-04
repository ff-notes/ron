{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

-- | RON model types
module RON.Types
    ( Atom (..)
    , Object (..)
    , ObjectId
    , ObjectPart (..)
    , Op (..)
    , OpTerm (..)
    , RawOp (..)
    , StateChunk (..)
    , StateFrame
    , UUID (..)
    , WireChunk (..)
    , WireFrame
    , WireReducedChunk (..)
    ) where

import           RON.Internal.Prelude

import           Data.Data (Data)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           RON.UUID (UUID (..))

-- | Atom — a payload element
data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Data, Eq, Generic, Hashable, Show)

-- | Raw op
data RawOp = RawOp
    { opType   :: UUID
        -- ^ type
    , opObject :: UUID
        -- ^ object id
    , op       :: Op
        -- ^ other keys and payload, that are common with reduced op
    }
    deriving (Data, Eq, Generic)

-- | “Reduced” op (op from reduced chunk)
data Op = Op
    { opEvent   :: UUID
        -- ^ event id (usually timestamp)
    , opRef     :: UUID
        -- ^ reference to other op; actual semantics depends on the type
    , opPayload :: [Atom]
        -- ^ payload
    }
    deriving (Data, Eq, Generic, Hashable, Show)

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

-- | Common reduced chunk
data WireReducedChunk = WireReducedChunk
    { wrcHeader :: RawOp
    , wrcBody   :: [Op]
    }
    deriving (Data, Eq, Generic, Show)

-- | Common chunk
data WireChunk = Raw RawOp | Value WireReducedChunk | Query WireReducedChunk
    deriving (Data, Eq, Generic, Show)

-- | Common frame
type WireFrame = [WireChunk]

-- | Op terminator
data OpTerm = TRaw | TReduced | THeader | TQuery
    deriving (Eq, Show)

-- | A pair of (type, object)
type ObjectId = (UUID, UUID)

-- | Reduced chunk representing an object state (i. e. high-level value)
data StateChunk = StateChunk
    { stateVersion :: UUID
    , stateBody    :: [Op]
    }
    deriving (Eq, Show)

-- | Frame containing only state chunks
type StateFrame = Map ObjectId StateChunk

-- | Reference to an object inside a frame.
data Object a = Object{objectId :: UUID, objectFrame :: StateFrame}
    deriving (Eq, Show)

-- | Specific field or item in an object, identified by UUID.
data ObjectPart obj part = ObjectPart
    {partObject :: UUID, partLocation :: UUID, partFrame :: StateFrame}
