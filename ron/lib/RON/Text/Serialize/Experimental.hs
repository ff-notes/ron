{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Text.Serialize.Experimental (serializeOpenFrame) where

import RON.Prelude

import Data.ByteString.Lazy.Char8 qualified as BSLC

import RON.Text.Serialize (serializeOpenOp)
import RON.Types (Op (opId), OpenFrame)
import RON.UUID qualified as UUID

serializeOpenFrame :: OpenFrame -> ByteStringL
serializeOpenFrame ops =
    BSLC.intercalate ",\n" opsSerialized <> ";\n"
  where
    opsSerialized = zipWith serializeOpenOp (UUID.zero : map (.opId) ops) ops
