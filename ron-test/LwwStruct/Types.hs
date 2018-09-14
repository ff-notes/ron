{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LwwStruct.Types where

import           RON.Internal.Prelude
import           RON.Schema
import           RON.Schema.TH

$(let
    tExample1 = StructLww
        { structName = "Example1"
        , structFields =
            [ "int1" -: Field (TAtom TAInteger) mempty
            , "set4" -: Field (TORSet (TStructLww tExample2)) mempty
            , "str2" -: Field (TRga char) mempty
            , "str3" -: Field (TAtom TAString) mempty
            ]
        , structAnnotations = mempty{saHaskellDeriving = ["Eq", "Show"]}
        }
    tExample2 = StructLww
        { structName = "Example2"
        , structFields = ["vv5" -: Field TVersionVector mempty]
        , structAnnotations = mempty{saHaskellDeriving = ["Eq", "Show"]}
        }
    in mkReplicated [DStructLww tExample1, DStructLww tExample2])
