module RON.Text.Common where

import           RON.Types (Op (..))
import           RON.UUID (zero)

opZero :: Op
opZero = Op
    { opType     = zero
    , opObject   = zero
    , opEvent    = zero
    , opLocation = zero
    , opPayload  = []
    }
