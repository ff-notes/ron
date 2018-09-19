{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema
    ( Declaration (..)
    , Field (..)
    , Opaque (..)
    , OpaqueAnnotations (..)
    , RonType (..)
    , Schema
    , StructAnnotations (..)
    , StructLww (..)
    , TAtom (..)
    , char
    , def
    , field
    , opaqueAtoms
    , opaqueObject
    , rgaString
    ) where

import           RON.Internal.Prelude

import           Data.Default (Default, def)

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAtom      TAtom
    | TOpaque    Opaque
    | TOption    RonType
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

newtype OpaqueAnnotations = OpaqueAnnotations{oaHaskellType :: Maybe Text}
    deriving (Default, Show)

char :: RonType
char = opaqueAtoms def{oaHaskellType = Just "Char"}

rgaString :: RonType
rgaString = TRga char

data Opaque =
    Opaque{opaqueIsObject :: Bool, opaqueAnnotations :: OpaqueAnnotations}
    deriving (Show)

opaqueObject :: OpaqueAnnotations -> RonType
opaqueObject = TOpaque . Opaque True

opaqueAtoms :: OpaqueAnnotations -> RonType
opaqueAtoms = TOpaque . Opaque False
