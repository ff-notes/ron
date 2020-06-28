{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Types.Experimental (CollectionName, ObjectRef (..), OpenFrame) where

import           RON.Prelude

import           Data.Typeable (typeRep)
import           Text.Show (showParen, showString, showsPrec)

import           RON.Types (Op, UUID)

type CollectionName = ByteString

-- | Reference to an object in a collection.
data ObjectRef a = ObjectRef CollectionName UUID

instance Typeable a => Show (ObjectRef a) where
  showsPrec p (ObjectRef c u) =
    showParen (p >= 11) $
        showString "ObjectRef @"
      . showsPrec 11 (typeRep $ Proxy @a)
      . showString " "
      . showsPrec 11 c
      . showString " "
      . showsPrec 11 u

type OpenFrame = [Op]
