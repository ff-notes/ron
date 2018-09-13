{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module RON.Schema
    ( Annotation (..)
    , Declaration (..)
    , HaskellType (..)
    , HaskellType1 (..)
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
                                      cxt, dataD, derivClause, listT, mkName,
                                      recC, sourceNoUnpack, sourceStrict,
                                      varBangType)
import qualified Language.Haskell.TH as TH

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
char = TAtom TAString // [AnnHaskellType HaskellChar]

data Annotated t = Ann t [Annotation]
    deriving (Show)

(//) :: t -> [Annotation] -> Annotated t
(//) = Ann

data Annotation
    = AnnHaskellDeriving Text
    | AnnHaskellType     HaskellType
    | AnnHaskellType1    HaskellType1
    deriving (Show)

data HaskellType = HaskellChar
    deriving (Show)

data HaskellType1 = HaskellHashSet | HaskellList
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
        Just hsType -> case hsType of
            HaskellChar -> conT ''Char
    TBuiltin b -> case b of
        TORSet itemType -> do
            let container = case fromMaybe HaskellList mHsType1 of
                    HaskellList    -> listT
                    HaskellHashSet -> conT ''HashSet
            appT container $ mkViewType itemType
        TRga   itemType -> case fromMaybe HaskellList mHsType1 of
            HaskellList -> appT listT $ mkViewType itemType
            t           -> fail $ "Type is not compatible with RGA: " ++ show t
        TVersionVector -> conT ''VersionVector
    TStructLww StructLww{..} -> conT $ mkNameT slName
  where
    mHsType  = lookupHaskellType  annotations
    mHsType1 = lookupHaskellType1 annotations

lookupHaskellType :: [Annotation] -> Maybe HaskellType
lookupHaskellType = asum . map go where
    go = \case
        AnnHaskellType t -> Just t
        _                -> Nothing

lookupHaskellType1 :: [Annotation] -> Maybe HaskellType1
lookupHaskellType1 = asum . map go where
    go = \case
        AnnHaskellType1 t -> Just t
        _                 -> Nothing

lookupHaskellDeriving :: [Annotation] -> [Text]
lookupHaskellDeriving = mapMaybe $ \case
    AnnHaskellDeriving d -> Just d
    _                    -> Nothing
