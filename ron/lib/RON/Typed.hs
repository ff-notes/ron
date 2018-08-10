{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Typed
    ( AsAtom (..)
    , Field (..)
    , Object (..)
    , Replicated (..)
    , initializeObject
    , lwwStructToStateChunk
    , lwwStructToStateOps
    , objectToStateOps
    ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text

import           RON.Event (Clock, getEventUuid)
import           RON.Types (Atom (..), Op, ReducedChunk, UUID)

data Object a = Object
    { objectId    :: !UUID
    , objectValue :: !a
    }
    deriving (Eq, Show)

class Replicated a where
    type View a

    initialize :: Clock m => View a -> m a

    view :: a -> View a

    toStateOps
        :: UUID  -- ^ this object id
        -> a
        -> Either String [Op]

    toStateChunk
        :: UUID  -- ^ this object id
        -> a
        -> Either String ReducedChunk

class AsAtom a where
    toAtom :: a -> Atom

instance AsAtom Char where
    toAtom c = AString $ Text.singleton c

instance AsAtom Int64 where
    toAtom = AInteger

objectToStateOps :: Replicated a => Object a -> Either String [Op]
objectToStateOps Object{objectId, objectValue} =
    toStateOps objectId objectValue

initializeObject :: (Replicated a, Clock m) => View a -> m (Object a)
initializeObject v = Object <$> getEventUuid <*> initialize v

data Field = forall a. Field Text a

lwwStructToStateOps :: Text -> [Field] -> UUID -> Either String [Op]
lwwStructToStateOps = undefined
    -- do
    --     bodyInt  <- objectToStateOps ts_int
    --     bodyText <- objectToStateOps ts_text
    --     pure $ header : bodyInt ++ bodyText
    --   where
    --     Object{objectId = int_id}  = ts_int
    --     Object{objectId = text_id} = ts_text
    --     header = Op{..}
    --     opObject = this
    --     opEvent  = this
    --     opLocation = UUID.zero
    --     opPayload =
    --         [ AString "TestStruct"
    --         , AString "int",  AUuid int_id
    --         , AString "text", AUuid text_id
    --         ]
    --     opType = lwwType

lwwStructToStateChunk :: Text -> [Field] -> UUID -> Either String ReducedChunk
lwwStructToStateChunk = undefined
