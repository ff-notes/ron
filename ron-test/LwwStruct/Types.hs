{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LwwStruct.Types where

import           RON.Internal.Prelude
import           RON.Schema
import           RON.Schema.TH

$(let
    example1 = StructLww "Example1"
        [ "int1" -: Field (TAtom TAInteger) mempty
        , "set4" -: Field (TORSet (TStructLww example2)) mempty
        , "str2" -: Field (TRga char) mempty
        , "str3" -: Field (TAtom TAString) mempty
        ]
        mempty{saHaskellDeriving = ["Eq", "Show"]}
    example2 = StructLww "Example2"
        ["vv5" -: Field TVersionVector mempty]
        mempty{saHaskellDeriving = ["Eq", "Show"]}
    in mkReplicated [DStructLww example1, DStructLww example2])
