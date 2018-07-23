{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Data.LWW where

import           Data.Maybe (fromJust)

import           RON.Event (Event, encodeEvent)
import           RON.Typed (AsAtom, Replicated, toAtom, toReducedOps)
import           RON.Types (Op (..), UUID)
import qualified RON.UUID as UUID

data LWW a = LWW
    { value :: !a
    , time  :: !Event
    }

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

instance AsAtom a => Replicated (LWW a) where
    toReducedOps this LWW{..} = do
        opEvent <-
            maybe (fail "LWW.time is a bad Event") pure $ encodeEvent time
        pure [Op{..}]
      where
        opType     = lwwType
        opObject   = this
        opLocation = UUID.zero
        opPayload  = [toAtom value]
