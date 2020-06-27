{-# LANGUAGE OverloadedStrings #-}

module RON.Text.Serialize.Experimental (serializePatch) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC

import           RON.Text.Serialize (serializeOpenOp)
import           RON.Types (Op (..))
import qualified RON.UUID as UUID

-- TODO rename to serializeFrame (serializeOpenFrame?)
serializePatch :: [Op] -> ByteStringL
serializePatch ops =
  BSLC.intercalate ",\n" opsSerialized <> ";\n"
  where
    opsSerialized = zipWith serializeOpenOp (UUID.zero : map opId ops) ops
