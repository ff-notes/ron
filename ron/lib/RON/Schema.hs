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
import           Language.Haskell.TH (Exp (VarE), appE, appT, bindS, conE, conP,
                                      conT, dataD, doE, funD, instanceD, letS,
                                      listE, noBindS, recC, recConE, tupE, varE,
                                      varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk, objectEncoding)
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

mkReplicated :: Schema -> TH.DecsQ
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

mkReplicatedStructLww :: Annotated StructLww -> TH.DecsQ
mkReplicatedStructLww (Ann StructLww{..} annotations) = do
    fields <-
        for (Map.assocs slFields) $ \(fieldName, fieldType) ->
            case UUID.mkName . BSC.pack $ Text.unpack fieldName of
                Just fieldNameUuid -> pure (fieldNameUuid, fieldName, fieldType)
                Nothing -> fail $
                    "Field name is not representable in RON: " ++ show fieldName
    let fieldsToPack = listE
            [ tupE [liftData fieldNameUuid, [| I |] `appE` var]
            | (fieldNameUuid, fieldName, fieldType) <- fields
            , let var = maybe id (appE . conE) (fieldWrapper fieldType) $
                    varE $ mkNameT fieldName
            ]
    sequence
        [ dataD
            (TH.cxt [])
            name
            []
            Nothing
            [ recC name
                [ TH.varBangType (mkNameT fieldName) $
                    TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                    mkViewType fieldType
                | (fieldName, fieldType) <- Map.assocs slFields
                ]
            ]
            [ TH.derivClause Nothing $
                map (conT . mkNameT) $ lookupHaskellDeriving annotations
            ]
        , instanceD
            (TH.cxt [])
            (conT ''Replicated `appT` conT name)
            [valD' 'encoding [| objectEncoding |]]
        , instanceD
            (TH.cxt [])
            (conT ''ReplicatedAsObject `appT` conT name)
            [ valD' 'objectOpType [| lwwType |]
            , funD
                'newObject
                [ clause'
                    [conP name . map (varP . mkNameT) $ Map.keys slFields] $
                    [| LWW.newFrame |] `appE` fieldsToPack
                ]
            , do
                obj   <- TH.newName "obj";   let objE   = varE obj
                frame <- TH.newName "frame"; let frameE = varE frame
                ops   <- TH.newName "ops";   let opsE   = varE ops
                let fieldsToUnpack =
                        [ bindS
                            (maybe fieldP (\w -> conP w [fieldP]) $
                                fieldWrapper fieldType) $
                            [| LWW.getField |]
                            `appE` liftData fieldNameUuid
                            `appE` opsE
                            `appE` frameE
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
                    [ clause' [varP obj] $
                        appE
                            [|
                                fmapL $
                                (++) $("getObject @" ++ slName' ++ ": ")
                            |] $
                        doE $ letS
                                [valD' frame $ [| objectFrame |] `appE` objE]
                            : bindS
                                (varP ops)
                                ([| getObjectStateChunk |] `appE` objE)
                            : fieldsToUnpack
                            ++ [noBindS $ [| pure |] `appE` construct]
                    ]
            ]
        ]
  where
    name = mkNameT slName
    slName' = Text.unpack slName

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: Annotated RonType -> TH.TypeQ
mkViewType (Ann typ annotations) = case typ of
    TAtom a -> case mHsType of
        Nothing -> case a of
            TAInteger -> conT ''Int64
            TAString  -> conT ''Text
        Just hsType -> conT $ mkNameT hsType
    TBuiltin b -> case b of
        TORSet item -> wrap item
        TRga   item -> wrap item
        TVersionVector -> conT ''VersionVector
    TStructLww StructLww{..} -> conT $ mkNameT slName
  where
    mHsType  = lookupHaskellType  annotations
    mHsType1 = lookupHaskellType1 annotations
    wrap it = conT (mkNameT $ fromMaybe "[]" mHsType1) `appT` mkViewType it

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

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

clause' :: [TH.PatQ] -> TH.ExpQ -> TH.ClauseQ
clause' pat body = TH.clause pat (TH.normalB body) []
