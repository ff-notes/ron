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
        , "opt5" -: field (TComposite $ TOption $ TObject $ TStructLww example1)
        , "opt6" -: field (TComposite $ TOption $ TAtom TAInteger)
        , "set4" -: field (TObject $ TORSet $ TObject $ TStructLww example2)
        , "str2" -: field rgaString
        , "str3" -: field (TAtom TAString)
        ]
        def{saHaskellDeriving = ["Eq", "Show"]}
    example2 = StructLww "Example2"
        ["vv5" -: field (TObject TVersionVector)]
        def{saHaskellDeriving = ["Eq", "Show"]}
    in mkReplicated [DStructLww example1, DStructLww example2])
