{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Typed
    ( AsAtom (..)
    , Object (..)
    , Replicated (..)
    , initializeObject
    , objectToStateChunk
    , objectToStateOps
    ) where

import           Data.Int (Int64)
import qualified Data.Text as Text

import           RON.Event (Clock, getEventUuid)
import           RON.Types (Atom (..), Op (..), ReducedChunk (..), UUID)

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
objectToStateOps Object{..} = toStateOps objectId objectValue

objectToStateChunk :: Replicated a => Object a -> Either String ReducedChunk
objectToStateChunk Object{..} = toStateChunk objectId objectValue

initializeObject :: (Replicated a, Clock m) => View a -> m (Object a)
initializeObject v = Object <$> getEventUuid <*> initialize v
