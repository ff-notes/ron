{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module RON.Schema
    ( Alias (..)
    , Annotations (..)
    , Declaration (..)
    , Field (..)
    , RonType (..)
    , StructLww (..)
    , TAtom (..)
    , mkReplicated
    , char
    ) where

import           RON.Internal.Prelude

import           Control.Error (fmapL)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (Exp (VarE), appE, appT, bindS, conE, conP,
                                      conT, dataD, doE, funD, instanceD, letS,
                                      listE, listT, noBindS, recC, recConE,
                                      tupE, varE, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk, objectEncoding)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (AsORSet (..), AsObjectMap (..))
import           RON.Data.RGA (AsRga (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Types (objectFrame)
import qualified RON.UUID as UUID

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
    { slName        :: Text
    , slFields      :: Map Text Field
    , slAnnotations :: Annotations
    }
    deriving (Show)

data Field = Field{fieldType :: RonType, fieldAnnotations :: Annotations}
    deriving (Show)

newtype Declaration = DStructLww StructLww

type Schema = [Declaration]

data Alias = Alias{aliasType :: RonType, aliasAnnotations :: Annotations}
    deriving (Show)

char :: RonType
char = TAlias Alias
    { aliasType = TAtom TAString
    , aliasAnnotations = mempty{annHaskellType = Just "Char"}
    }

data Annotations = Annotations
    { annHaskellDeriving :: Set Text
    , annHaskellType     :: Maybe Text
    }
    deriving (Show)

instance Semigroup Annotations where
    Annotations a1 a2 <> Annotations b1 b2 = Annotations (a1 <> b1) (a2 <> b2)

instance Monoid Annotations where
    mempty = Annotations
        { annHaskellDeriving = mempty
        , annHaskellType     = mempty
        }

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

fieldWrapper :: RonType -> Maybe TH.Name
fieldWrapper typ = case typ of
    TAlias     _            -> Nothing
    TAtom      _            -> Nothing
    TORSet     item
        | isObjectType item -> Just 'AsObjectMap
        | otherwise         -> Just 'AsORSet
    TRga       _            -> Just 'AsRga
    TStructLww _            -> Nothing
    TVersionVector          -> Nothing

mkReplicatedStructLww :: StructLww -> TH.DecsQ
mkReplicatedStructLww StructLww{..} = do
    fields <- for (Map.assocs slFields) $ \(fieldName, fieldType) ->
        case UUID.mkName . BSC.pack $ Text.unpack fieldName of
            Just fieldNameUuid -> pure (fieldNameUuid, fieldName, fieldType)
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show fieldName
    let fieldsToPack = listE
            [ tupE [liftData fieldNameUuid, [| I |] `appE` var]
            | (fieldNameUuid, fieldName, Field fieldType _) <- fields
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
            | (fieldNameUuid, fieldName, Field fieldType _) <- fields
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
                | (fieldName, Field fieldType _) <- Map.assocs slFields
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
                    appE [| fmapL $ (++) $("getObject @" ++ slName' ++ ":\n") |]
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

mkViewType :: RonType -> TH.TypeQ
mkViewType typ = case typ of
    TAlias Alias{aliasAnnotations = Annotations{..}, ..} ->
        case annHaskellType of
            Nothing     -> mkViewType aliasType
            Just hsType -> conT $ mkNameT hsType
    TAtom atom -> case atom of
        TAInteger -> conT ''Int64
        TAString  -> conT ''Text
    TORSet item -> wrapList item
    TRga   item -> wrapList item
    TStructLww StructLww{..} -> conT $ mkNameT slName
    TVersionVector -> conT ''VersionVector
  where
    wrapList = appT listT . mkViewType

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

clause' :: [TH.PatQ] -> TH.ExpQ -> TH.ClauseQ
clause' pat body = TH.clause pat (TH.normalB body) []

isObjectType :: RonType -> Bool
isObjectType = \case
    TAlias     a   -> isObjectType $ aliasType a
    TAtom      _   -> False
    TORSet     _   -> True
    TRga       _   -> True
    TStructLww _   -> True
    TVersionVector -> True
