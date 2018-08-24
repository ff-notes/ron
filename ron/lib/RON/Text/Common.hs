module RON.Text.Common where

import           RON.Types (Op (Op), ROp (ROp), opObject, opR, opType, ropEvent,
                            ropLocation, ropPayload)
import           RON.UUID (zero)

opZero :: Op
opZero = Op
    { opType   = zero
    , opObject = zero
    , opR      = ROp{ropEvent = zero, ropLocation = zero, ropPayload = []}
    }
