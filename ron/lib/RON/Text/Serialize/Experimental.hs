{-# LANGUAGE OverloadedStrings #-}

module RON.Text.Serialize.Experimental (serializeOpenFrame) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC

import           RON.Text.Serialize (serializeOpenOp)
import           RON.Types (Op (opId))
import           RON.Types.Experimental (OpenFrame)
import qualified RON.UUID as UUID

serializeOpenFrame :: OpenFrame -> ByteStringL
serializeOpenFrame ops =
  BSLC.intercalate ",\n" opsSerialized <> ";\n"
  where
    opsSerialized = zipWith serializeOpenOp (UUID.zero : map opId ops) ops
