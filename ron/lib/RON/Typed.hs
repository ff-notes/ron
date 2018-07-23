{-# LANGUAGE NamedFieldPuns #-}

module RON.Typed
    ( AsAtom (..)
    , Object (..)
    , Replicated (..)
    , objectToReducedOps
    ) where

import           Data.Int (Int64)
import qualified Data.Text as Text

import           RON.Types (Atom (..), Op, UUID)

data Object a = Object
    { objectId      :: !UUID
    , objectValue   :: !a
    }

class Replicated a where
    toReducedOps
        :: UUID  -- ^ this object id
        -> a
        -> Either String [Op]

class AsAtom a where
    toAtom :: a -> Atom

instance AsAtom Char where
    toAtom c = AString $ Text.singleton c

instance AsAtom Int64 where
    toAtom = AInteger

objectToReducedOps :: Replicated a => Object a -> Either String [Op]
objectToReducedOps Object{objectId, objectValue} =
    toReducedOps objectId objectValue
