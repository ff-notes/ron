{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema
    ( Alias (..)
    , AliasAnnotations (..)
    , Declaration (..)
    , Field (..)
    , RonType (..)
    , Schema
    , StructAnnotations (..)
    , StructLww (..)
    , TAtom (..)
    , alias
    , char
    , def
    , field
    , option
    , rgaString
    ) where

import           RON.Internal.Prelude

import           Data.Default (Default, def)

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAlias     Alias
    | TAtom      TAtom
    | TAtomTuple [TAtom]
    | TORSet     RonType
    | TRga       RonType
    | TStructLww StructLww
    | TVersionVector
    deriving (Show)

data StructLww = StructLww
    { structName        :: Text
    , structFields      :: Map Text Field
    , structAnnotations :: StructAnnotations
    }
    deriving (Show)

data StructAnnotations = StructAnnotations
    {saHaskellDeriving :: Set Text, saHaskellFieldPrefix :: Text}
    deriving (Show)

instance Default StructAnnotations where def = StructAnnotations def ""

data Field = Field{fieldType :: RonType, fieldAnnotations :: FieldAnnotations}
    deriving (Show)

field :: RonType -> Field
field fieldType = Field{fieldType, fieldAnnotations = def}

data FieldAnnotations = FieldAnnotations
    deriving (Show)

instance Default FieldAnnotations where def = FieldAnnotations

newtype Declaration = DStructLww StructLww

type Schema = [Declaration]

data Alias = Alias{aliasType :: RonType, aliasAnnotations :: AliasAnnotations}
    deriving (Show)

data AliasAnnotations =
    AliasAnnotations{aaHaskellType :: Maybe Text, aaOption :: Bool}
    deriving (Show)

instance Default AliasAnnotations where def = AliasAnnotations def False

char :: RonType
char = alias (TAtom TAString) def{aaHaskellType = Just "Char"}

rgaString :: RonType
rgaString = TRga char

alias :: RonType -> AliasAnnotations -> RonType
alias t a = TAlias $ Alias t a

option :: RonType -> RonType
option typ = alias typ def{aaOption = True}
