{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module RON.Schema
    ( Annotation (..)
    , Declaration (..)
    , RonType (..)
    , StructLww (..)
    , TAtom (..)
    , TBuiltin (..)
    , mkReplicated
    , char
    , (//)
    ) where

import           RON.Internal.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (DecsQ, TypeQ, appT, bang, bangType, conT,
                                      cxt, dataD, derivClause, instanceD,
                                      mkName, normalB, recC, sourceNoUnpack,
                                      sourceStrict, valD, varBangType, varP)
import qualified Language.Haskell.TH as TH

import           RON.Data (Replicated (..))
import           RON.Data.VersionVector (VersionVector)

data TAtom = TAInteger | TAString
    deriving (Show)

data TBuiltin
    = TORSet (Annotated RonType)
    | TRga (Annotated RonType)
    | TVersionVector
    deriving (Show)

data RonType
    = TAtom      TAtom
    | TBuiltin   TBuiltin
    | TStructLww StructLww
    deriving (Show)

data StructLww = StructLww
    { slName    :: Text
    , slFields  :: Map Text (Annotated RonType)
    }
    deriving (Show)

newtype Declaration = DStructLww StructLww

type Schema = [Annotated Declaration]

char :: Annotated RonType
char = TAtom TAString // [HaskellType "Char"]

data Annotated t = Ann t [Annotation]
    deriving (Show)

(//) :: t -> [Annotation] -> Annotated t
(//) = Ann

data Annotation
    = HaskellDeriving Text
    | HaskellType     Text
    | HaskellType1    Text
    deriving (Show)

mkReplicated :: Schema -> DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl (Ann decl annotations) = case decl of
        DStructLww s -> mkReplicatedStructLww (s // annotations)

mkReplicatedStructLww :: Annotated StructLww -> DecsQ
mkReplicatedStructLww (Ann StructLww{..} annotations) = sequence
    [ dataD
        (cxt [])
        name
        []
        Nothing
        [ recC name
            [ varBangType (mkNameT fieldName) $
                bangType (bang sourceNoUnpack sourceStrict) $
                mkViewType fieldType
            | (fieldName, fieldType) <- Map.assocs slFields
            ]
        ]
        [ derivClause Nothing $
            map (conT . mkNameT) $ lookupHaskellDeriving annotations
        ]
    , instanceD
        (cxt [])
        (appT (conT ''Replicated) (conT name))
        [valD (varP 'encoding) (normalB [| objectEncoding |]) []]
    ]
  where
    name = mkNameT slName

mkNameT :: Text -> TH.Name
mkNameT = mkName . Text.unpack

mkViewType :: Annotated RonType -> TypeQ
mkViewType (Ann typ annotations) = case typ of
    TAtom a -> case mHsType of
        Nothing -> case a of
            TAInteger -> conT ''Int64
            TAString  -> conT ''Text
        Just hsType -> conT $ mkNameT hsType
    TBuiltin b -> case b of
        TORSet item ->
            appT (conT . mkNameT $ fromMaybe "[]" mHsType1) (mkViewType item)
        TRga   item ->
            appT (conT . mkNameT $ fromMaybe "[]" mHsType1) (mkViewType item)
        TVersionVector -> conT ''VersionVector
    TStructLww StructLww{..} -> conT $ mkNameT slName
  where
    mHsType  = lookupHaskellType  annotations
    mHsType1 = lookupHaskellType1 annotations

lookupHaskellType :: [Annotation] -> Maybe Text
lookupHaskellType = asum . map go where
    go = \case
        HaskellType t -> Just t
        _             -> Nothing

lookupHaskellType1 :: [Annotation] -> Maybe Text
lookupHaskellType1 = asum . map go where
    go = \case
        HaskellType1 t -> Just t
        _              -> Nothing

lookupHaskellDeriving :: [Annotation] -> [Text]
lookupHaskellDeriving = mapMaybe $ \case
    HaskellDeriving d -> Just d
    _                 -> Nothing
