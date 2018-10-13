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
        [ "int1" -: field (TAtom TAInteger)
        , "opt5" -: field (TOption (TStructLww example1))
        , "set4" -: field (TORSet (TStructLww example2))
        , "str2" -: field (TRga char)
        , "str3" -: field (TAtom TAString)
        ]
        def{saHaskellDeriving = ["Eq", "Show"]}
    example2 = StructLww "Example2"
        ["vv5" -: field TVersionVector]
        def{saHaskellDeriving = ["Eq", "Show"]}
    in mkReplicated [DStructLww example1, DStructLww example2])
