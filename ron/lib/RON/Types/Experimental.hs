{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Types.Experimental (Patch (..), Ref (..)) where

import RON.Prelude

import Data.Typeable (typeRep)
import Text.Show (showParen, showString, showsPrec)

import RON.Types (Op, UUID)

{- | References to a RON object or a subobject
TODO hide data constructor in Internal module
-}
newtype Ref a = Ref {object :: UUID}

instance (Typeable a) => Show (Ref a) where
    showsPrec a Ref{object} =
        showParen (a >= 11) $
            showString "Ref @"
                . showsPrec 11 (typeRep $ Proxy @a)
                . showString " "
                . showsPrec 11 object

data Patch = Patch
    { object :: UUID
    , log :: NonEmpty Op
    }
    deriving (Show)
