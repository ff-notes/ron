module String (s) where

import           RON.Prelude

import           Language.Haskell.TH (litE, stringL)
import           Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter), quoteDec,
                                            quoteExp, quotePat, quoteType)

s :: QuasiQuoter
s = QuasiQuoter
    { quoteDec = undefined
    , quoteExp = litE . stringL
    , quotePat = undefined
    , quoteType = undefined
    }
