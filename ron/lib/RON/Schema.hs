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

import           Control.Error (fmapL)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (DecsQ, Exp (VarE), TypeQ, appE, appT,
                                      bang, bangType, bindS, clause, conE, conP,
                                      conT, cxt, dataD, derivClause, doE, funD,
                                      instanceD, letS, listE, mkName, newName,
                                      noBindS, normalB, recC, recConE,
                                      sourceNoUnpack, sourceStrict, tupE, valD,
                                      varBangType, varE, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (AsORSet (..))
import           RON.Data.RGA (AsRga (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Types (objectFrame)
import qualified RON.UUID as UUID

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

fieldWrapper :: Annotated RonType -> Maybe TH.Name
fieldWrapper (Ann typ _) = case typ of
    TAtom      _ -> Nothing
    TBuiltin   b -> case b of
        TORSet _       -> Just 'AsORSet
        TRga   _       -> Just 'AsRga
        TVersionVector -> Nothing
    TStructLww _ -> Nothing

mkReplicatedStructLww :: Annotated StructLww -> DecsQ
mkReplicatedStructLww (Ann StructLww{..} annotations) = do
    fields <-
        for (Map.assocs slFields) $ \(fieldName, fieldType) ->
            case UUID.mkName . BSC.pack $ Text.unpack fieldName of
                Just fieldNameUuid -> pure (fieldNameUuid, fieldName, fieldType)
                Nothing -> fail $
                    "Field name is not representable in RON: " ++ show fieldName
    sequence
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
        , instanceD
            (cxt [])
            (appT (conT ''ReplicatedAsObject) (conT name))
            [ valD (varP 'objectOpType) (normalB [| lwwType |]) []
            , let
                fieldsToPack = listE
                    [ tupE
                        [ liftData fieldNameUuid
                        , appE [| I |] $
                            maybe id (appE . conE) (fieldWrapper fieldType) $
                            varE $ mkNameT fieldName
                        ]
                    | (fieldNameUuid, fieldName, fieldType) <- fields
                    ]
                in
                funD
                    'newObject
                    [ clause
                        [conP name . map (varP . mkNameT) $ Map.keys slFields]
                        (normalB $ appE [| LWW.newFrame |] fieldsToPack)
                        []
                    ]
            , do
                obj   <- newName "obj";   let objE   = varE obj
                frame <- newName "frame"; let frameE = varE frame
                ops   <- newName "ops";   let opsE   = varE ops
                let fieldsToUnpack =
                        [ bindS
                            (maybe fieldP (\w -> conP w [fieldP]) $
                                fieldWrapper fieldType) $
                            appE
                                (appE
                                    (appE [| LWW.getField |] $
                                        liftData fieldNameUuid)
                                    opsE)
                                frameE
                        | (fieldNameUuid, fieldName, fieldType) <- fields
                        , let fieldP = varP $ mkNameT fieldName
                        ]
                let construct =
                        recConE
                            name
                            [ pure (fieldName, VarE fieldName)
                            | field <- Map.keys slFields
                            , let fieldName = mkNameT field
                            ]
                funD
                    'getObject
                    [ clause
                        [varP obj]
                        ( normalB $
                            appE
                                [|
                                    fmapL $
                                    (++) $("getObject @" ++ slName' ++ ": ")
                                |] $
                            doE $ letS
                                    [ valD
                                        (varP frame)
                                        (normalB $ appE [| objectFrame |] objE)
                                        []
                                    ]
                                : bindS
                                    (varP ops)
                                    (appE [| getObjectStateChunk |] objE)
                                : fieldsToUnpack
                                ++ [noBindS $ appE [| pure |] construct]
                        )
                        []
                    ]
            ]
        ]
  where
    name = mkNameT slName
    slName' = Text.unpack slName

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
