{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Typed
    ( AsAtom (..)
    , Object (..)
    , Replicated (..)
    , initializeObject
    , objectFromStateChunk
    , objectFromStateOps
    , objectToStateChunk
    , objectToStateOps
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import qualified Data.Text as Text

import           RON.Event (Clock, getEventUuid)
import           RON.Types (Atom (..), Op (..), RChunk (..), UUID)

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
        :: UUID  -- ^ 'opObject'
        -> a
        -> Either String [Op]

    toStateChunk
        :: UUID  -- ^ 'opObject'
        -> a
        -> Either String RChunk

    fromStateOps
        :: UUID               -- ^ 'opObject'
        -> [Op]               -- ^ this object's direct ops
        -> HashMap UUID [Op]  -- ^ body groupped by 'opObject'
        -> Either String a

    fromStateChunk
        :: Op                 -- ^ header
        -> [Op]               -- ^ this object's direct ops
        -> HashMap UUID [Op]  -- ^ body groupped by 'opObject'
        -> Either String a

class AsAtom a where
    toAtom   :: a -> Atom
    fromAtom :: Atom -> Maybe a

instance AsAtom Char where
    toAtom c = AString $ Text.singleton c
    fromAtom = \case
        AString (Text.uncons -> Just (c, "")) -> Just c
        _                                     -> Nothing

instance AsAtom Int64 where
    toAtom = AInteger
    fromAtom = \case
        AInteger i -> Just i
        _          -> Nothing

objectToStateOps :: Replicated a => Object a -> Either String [Op]
objectToStateOps Object{..} = toStateOps objectId objectValue

objectToStateChunk :: Replicated a => Object a -> Either String RChunk
objectToStateChunk Object{..} = toStateChunk objectId objectValue

initializeObject :: (Replicated a, Clock m) => View a -> m (Object a)
initializeObject v = Object <$> getEventUuid <*> initialize v

objectFromStateChunk :: Replicated a => RChunk -> Either String (Object a)
objectFromStateChunk chunk = do
    objectValue <- fromStateChunk chunkHeader ownOps ops
    pure Object{objectId, objectValue}
  where
    RChunk{chunkHeader, chunkBody} = chunk
    Op{opObject = objectId} = chunkHeader
    ops = HM.fromListWith (flip (++))
        [(opObject, [op]) | op @ Op{opObject} <- chunkBody]
    ownOps = HM.lookupDefault [] objectId ops

objectFromStateOps
    :: Replicated a
    => UUID -> [Op] -> HashMap UUID [Op] -> Either String (Object a)
objectFromStateOps objectId ownOps ops = do
    objectValue <- fromStateOps objectId ownOps ops
    pure Object{objectId, objectValue}
