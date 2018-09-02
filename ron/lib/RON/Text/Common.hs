module RON.Text.Common where

import           RON.Types (Op (..), Op' (..))
import           RON.UUID (zero)

opZero :: Op
opZero = Op
    { opType   = zero
    , opObject = zero
    , op'      = Op'{opEvent = zero, opRef = zero, opPayload = []}
    }
