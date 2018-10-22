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
        [ "int1" -: field atomInteger
        , "opt5" -: field (option $ structLww example1)
        , "opt6" -: field (option atomInteger)
        , "set4" -: field (orSet $ structLww example2)
        , "str2" -: field rgaString
        , "str3" -: field atomString
        ]
        def{saHaskellDeriving = ["Eq", "Show"]}
    example2 = StructLww "Example2" ["vv5" -: field versionVector]
        def{saHaskellDeriving = ["Eq", "Show"]}
    in mkReplicated [DStructLww example1, DStructLww example2])
