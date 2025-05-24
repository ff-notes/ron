{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Cxx.Std (
    String,
    stdCtx,
) where

import           Prelude hiding (String)

import qualified Data.Map.Strict as Map
import           Language.C.Inline.Context (Context, ctxTypesTable)
import           Language.C.Types (TypeSpecifier (TypeName))

-- | Class @std::string@
data String

stdCtx :: Context
stdCtx = mempty
    { ctxTypesTable = Map.singleton (TypeName "std_string") [t| String |]
    }
