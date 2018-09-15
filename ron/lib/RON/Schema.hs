{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , char
    , field
    ) where

import           RON.Internal.Prelude

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAlias     Alias
    | TAtom      TAtom
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

newtype StructAnnotations = StructAnnotations{saHaskellDeriving :: Set Text}
    deriving newtype (Monoid, Semigroup)
    deriving stock (Show)

data Field = Field{fieldType :: RonType, fieldAnnotations :: FieldAnnotations}
    deriving (Show)

field :: RonType -> Field
field fieldType = Field{fieldType, fieldAnnotations = mempty}

newtype FieldAnnotations = FieldAnnotations ()
    deriving newtype (Monoid, Semigroup)
    deriving stock (Show)

newtype Declaration = DStructLww StructLww

type Schema = [Declaration]

data Alias = Alias{aliasType :: RonType, aliasAnnotations :: AliasAnnotations}
    deriving (Show)

newtype AliasAnnotations = AliasAnnotations{aaHaskellType :: Maybe Text}
    deriving newtype (Monoid, Semigroup)
    deriving stock (Show)

char :: RonType
char = TAlias Alias
    { aliasType = TAtom TAString
    , aliasAnnotations = mempty{aaHaskellType = Just "Char"}
    }
