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
    TEnum (..),
    TComposite (..),
    TObject (..),
    opaqueAtoms,
    opaqueAtoms_,
    opaqueObject,
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

data TComposite
    = TOption RonType
    | TEnum   TEnum
    deriving (Show)

data TEnum = Enum {enumName :: Text, enumItems :: [Text]}
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

data FieldAnnotations = FieldAnnotations
    deriving (Show)

instance Default FieldAnnotations where
    def = FieldAnnotations

data Declaration = DEnum TEnum | DOpaque Opaque | DStructLww StructLww

type Schema = [Declaration]

newtype OpaqueAnnotations = OpaqueAnnotations{oaHaskellType :: Maybe Text}
    deriving (Default, Show)

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

opaqueAtoms_ :: Text -> RonType
opaqueAtoms_ name = TOpaque $ Opaque False name def
