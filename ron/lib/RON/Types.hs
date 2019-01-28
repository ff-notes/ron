{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | RON model types
module RON.Types
    ( Atom (..)
    , Object (..)
    , ObjectPart (..)
    , Op (..)
    , OpTerm (..)
    , ClosedOp (..)
    , StateChunk (..)
    , StateFrame
    , UUID (..)
    , WireChunk (..)
    , WireFrame
    , WireReducedChunk (..)
    ) where

import qualified Text.Show

import           RON.UUID (UUID (..))

-- | Atom — a payload element
data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Data, Eq, Generic, Hashable, Show)

-- | Closed op
data ClosedOp = ClosedOp
    { reducerId :: UUID
        -- ^ type
    , objectId  :: UUID
        -- ^ object id
    , op        :: Op
        -- ^ other keys and payload, that are common with reduced op
    }
    deriving (Data, Eq, Generic)

-- | Open op (operation)
data Op = Op
    { opId      :: UUID
        -- ^ event id (usually timestamp)
    , refId     :: UUID
        -- ^ reference to other op; actual semantics depends on the type
    , payload :: [Atom]
        -- ^ payload
    }
    deriving (Data, Eq, Generic, Hashable, Show)

instance Show ClosedOp where
    show ClosedOp{reducerId, objectId, op = Op{opId, refId, payload}} =
        unwords
            [ "ClosedOp"
            , insert '*' $ show reducerId
            , insert '#' $ show objectId
            , insert '@' $ show opId
            , insert ':' $ show refId
            , show payload
            ]
      where
        insert k = \case
            []   -> [k]
            c:cs -> c:k:cs

-- | Common reduced chunk
data WireReducedChunk = WireReducedChunk
    { wrcHeader :: ClosedOp
    , wrcBody   :: [Op]
    }
    deriving (Data, Eq, Generic, Show)

-- | Common chunk
data WireChunk =
    Closed ClosedOp | Value WireReducedChunk | Query WireReducedChunk
    deriving (Data, Eq, Generic, Show)

-- | Common frame
type WireFrame = [WireChunk]

-- | Op terminator
data OpTerm = TClosed | TReduced | THeader | TQuery
    deriving (Eq, Show)

-- | Reduced chunk representing an object state (i. e. high-level value)
data StateChunk = StateChunk
    { stateType    :: UUID
    , stateVersion :: UUID
    , stateBody    :: [Op]
    }
    deriving (Eq, Show)

-- | Frame containing only state chunks
type StateFrame = Map UUID StateChunk

-- | Reference to an object inside a frame.
data Object a = Object{id :: UUID, frame :: StateFrame}
    deriving (Eq, Show)

-- | Specific field or item in an object, identified by UUID.
data ObjectPart obj part = ObjectPart
    {partObject :: UUID, partLocation :: UUID, partFrame :: StateFrame}
