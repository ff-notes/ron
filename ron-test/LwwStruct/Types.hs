{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LwwStruct.Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           RON.Schema
import           RON.Schema.TH

$(let
    tExample1 = StructLww
        { structName = "Example1"
        , structFields = Map.fromList
            [ ("int1", Field (TAtom TAInteger) mempty)
            , ("set4", Field (TORSet (TStructLww tExample2)) mempty)
            , ("str2", Field (TRga char) mempty)
            , ("str3", Field (TAtom TAString) mempty)
            ]
        , structAnnotations =
            mempty{saHaskellDeriving = Set.fromList ["Eq", "Show"]}
        }
    tExample2 = StructLww
        { structName = "Example2"
        , structFields = Map.fromList [("vv5", Field TVersionVector mempty)]
        , structAnnotations =
            mempty{saHaskellDeriving = Set.fromList ["Eq", "Show"]}
        }
    in mkReplicated [DStructLww tExample1, DStructLww tExample2])
