{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema (
    CaseTransform (..),
    Declaration (..),
    Field (..),
    FieldAnnotations (..),
    Opaque (..),
    OpaqueAnnotations (..),
    RonType (..),
    Schema,
    StructAnnotations (..),
    StructLww (..),
    TAtom (..),
    TComposite (..),
    TObject (..),
    atomInteger,
    atomString,
    boole,
    char,
    def,
    field,
    opaqueAtoms,
    opaqueObject,
    option,
    orSet,
    rgaString,
    structLww,
    versionVector,
) where

import           RON.Internal.Prelude

import           Data.Default (Default, def)

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAtom      TAtom
    | TComposite TComposite
    | TObject    TObject
    | TOpaque    Opaque
    deriving (Show)

newtype TComposite
    = TOption RonType
    deriving (Show)

data TObject
    = TORSet     RonType
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
    { saHaskellFieldPrefix        :: Text
    , saHaskellFieldCaseTransform :: Maybe CaseTransform
    }
    deriving (Show)

data CaseTransform = TitleCase
    deriving (Show)

instance Default StructAnnotations where def = StructAnnotations "" Nothing

data Field = Field{fieldType :: RonType, fieldAnnotations :: FieldAnnotations}
    deriving (Show)

field :: RonType -> Field
field fieldType = Field{fieldType, fieldAnnotations = def}

data FieldAnnotations = FieldAnnotations
    deriving (Show)

instance Default FieldAnnotations where
    def = FieldAnnotations

data Declaration = DOpaque Opaque | DStructLww StructLww

type Schema = [Declaration]

newtype OpaqueAnnotations = OpaqueAnnotations{oaHaskellType :: Maybe Text}
    deriving (Default, Show)

char :: RonType
char = opaqueAtoms "Char" def{oaHaskellType = Just "Char"}

rgaString :: RonType
rgaString = TObject $ TRga char

data Opaque = Opaque
    { opaqueIsObject    :: Bool
    , opaqueName        :: Text
    , opaqueAnnotations :: OpaqueAnnotations
    }
    deriving (Show)

opaqueObject :: Text -> OpaqueAnnotations -> RonType
opaqueObject name = TOpaque . Opaque True name

opaqueAtoms :: Text -> OpaqueAnnotations -> RonType
opaqueAtoms name = TOpaque . Opaque False name

option :: RonType -> RonType
option = TComposite . TOption

structLww :: StructLww -> RonType
structLww = TObject . TStructLww

atomString :: RonType
atomString = TAtom TAString

atomInteger :: RonType
atomInteger = TAtom TAInteger

orSet :: RonType -> RonType
orSet = TObject . TORSet

versionVector :: RonType
versionVector = TObject TVersionVector

boole :: RonType
boole = opaqueAtoms "Boole" def{oaHaskellType = Just "Bool"}
