module RON.Text.Common where

import           RON.Types (Op (..), RawOp (..))
import           RON.UUID (zero)

opZero :: RawOp
opZero = RawOp
    { opType   = zero
    , opObject = zero
    , op       = Op{opEvent = zero, opRef = zero, opPayload = []}
    }
