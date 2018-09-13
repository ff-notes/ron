{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module RON.Schema
    ( Annotations (..)
    , Declaration (..)
    , RonType (..)
    , StructLww (..)
    , TAtom (..)
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

data RonType
    = TAtom      TAtom
    | TORSet     (Annotated RonType)
    | TRga       (Annotated RonType)
    | TStructLww StructLww
    | TVersionVector
    deriving (Show)

data StructLww = StructLww
    { slName        :: Text
    , slFields      :: Map Text (Annotated RonType)
    , slAnnotations :: Annotations
    }
    deriving (Show)

newtype Declaration = DStructLww StructLww

type Schema = [Declaration]

char :: Annotated RonType
char = TAtom TAString // mempty{annHaskellType = Just "Char"}

data Annotated t = Ann t Annotations
    deriving (Show)

(//) :: t -> Annotations -> Annotated t
(//) = Ann

data Annotations = Annotations
    { annHaskellDeriving :: Set Text
    , annHaskellType     :: Maybe Text
    , annHaskellType1    :: Maybe Text
    }
    deriving (Show)

instance Semigroup Annotations where
    Annotations a1 a2 a3 <> Annotations b1 b2 b3 =
        Annotations (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid Annotations where
    mempty = Annotations
        { annHaskellDeriving = mempty
        , annHaskellType     = mempty
        , annHaskellType1    = mempty
        }

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

fieldWrapper :: Annotated RonType -> Maybe TH.Name
fieldWrapper (Ann typ _) = case typ of
    TAtom      _   -> Nothing
    TORSet     _   -> Just 'AsORSet
    TRga       _   -> Just 'AsRga
    TStructLww _   -> Nothing
    TVersionVector -> Nothing

mkReplicatedStructLww :: StructLww -> TH.DecsQ
mkReplicatedStructLww StructLww{..} = do
    fields <- for (Map.assocs slFields) $ \(fieldName, fieldType) ->
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
    obj   <- TH.newName "obj";   let objE   = varE obj
    frame <- TH.newName "frame"; let frameE = varE frame
    ops   <- TH.newName "ops";   let opsE   = varE ops
    let fieldsToUnpack =
            [ bindS var $
                [| LWW.getField |] `appE` liftData fieldNameUuid
                `appE` opsE `appE` frameE
            | (fieldNameUuid, fieldName, fieldType) <- fields
            , let
                fieldP = varP $ mkNameT fieldName
                var = maybe fieldP (\w -> conP w [fieldP]) $
                    fieldWrapper fieldType
            ]
    sequence
        [ dataD (TH.cxt []) name [] Nothing
            [recC name
                [ TH.varBangType (mkNameT fieldName) $
                    TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                    mkViewType fieldType
                | (fieldName, fieldType) <- Map.assocs slFields
                ]]
            [TH.derivClause Nothing . map (conT . mkNameT) $
                toList annHaskellDeriving]
        , instanceD (TH.cxt []) (conT ''Replicated `appT` conT name)
            [valD' 'encoding [| objectEncoding |]]
        , instanceD
            (TH.cxt [])
            (conT ''ReplicatedAsObject `appT` conT name)
            [ valD' 'objectOpType [| lwwType |]
            , funD 'newObject
                [clause'
                    [conP name . map (varP . mkNameT) $ Map.keys slFields] $
                    [| LWW.newFrame |] `appE` fieldsToPack]
            , funD 'getObject
                [clause' [varP obj] $
                    appE [| fmapL $ (++) $("getObject @" ++ slName' ++ ": ") |]
                    $ doE
                    $ letS [valD' frame $ [| objectFrame |] `appE` objE]
                    : bindS (varP ops) ([| getObjectStateChunk |] `appE` objE)
                    : fieldsToUnpack
                    ++ [noBindS $ [| pure |] `appE` cons]]
            ]
        ]
  where
    Annotations{..} = slAnnotations
    name = mkNameT slName
    slName' = Text.unpack slName
    cons = recConE
        name
        [ pure (fieldName, VarE fieldName)
        | field <- Map.keys slFields, let fieldName = mkNameT field
        ]


mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: Annotated RonType -> TH.TypeQ
mkViewType (Ann typ Annotations{..}) = case typ of
    TAtom a -> case annHaskellType of
        Nothing -> case a of
            TAInteger -> conT ''Int64
            TAString  -> conT ''Text
        Just hsType -> conT $ mkNameT hsType
    TORSet item -> wrap item
    TRga   item -> wrap item
    TStructLww StructLww{..} -> conT $ mkNameT slName
    TVersionVector -> conT ''VersionVector
  where
    wrap = appT (conT . mkNameT $ fromMaybe "[]" annHaskellType1) . mkViewType

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

clause' :: [TH.PatQ] -> TH.ExpQ -> TH.ClauseQ
clause' pat body = TH.clause pat (TH.normalB body) []
