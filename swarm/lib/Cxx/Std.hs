{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cxx.Std (
    String (..),
    stdCtx,
) where

import           Prelude hiding (String)

import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy)
import           Foreign (ForeignPtr)
import           Language.C.Inline.Context (Context, ctxTypesTable)
import           Language.C.Types (TypeSpecifier (TypeName))

-- | Class @std::string@
newtype String = String (ForeignPtr (Proxy String))

stdCtx :: Context
stdCtx = mempty
    { ctxTypesTable = Map.singleton (TypeName "std_string") [t| Proxy String |]
    }
