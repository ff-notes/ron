{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH
    ( mkReplicated
    ) where

import           RON.Internal.Prelude

import           Control.Error (fmapL)
import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (MonadState, StateT)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (Exp (VarE), bindS, conE, conP, conT,
                                      dataD, doE, letS, listE, noBindS, recC,
                                      recConE, sigD, varE, varP, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (lift, liftData)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk, objectEncoding)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (ORSet (..), ObjectORSet (..))
import           RON.Data.RGA (RGA (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Event (Clock)
import           RON.Schema (Declaration (..), Field (..),
                             FieldAnnotations (..), Opaque (..),
                             OpaqueAnnotations (..), RonType (..), Schema,
                             StructAnnotations (..), StructLww (..), TAtom (..))
import           RON.Types (Object (..), UUID)
import qualified RON.UUID as UUID

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

fieldWrapperC :: RonType -> Maybe TH.Name
fieldWrapperC typ = case typ of
    TAtom      _            -> Nothing
    TOpaque    _            -> Nothing
    TORSet     item
        | isObjectType item -> Just 'ObjectORSet
        | otherwise         -> Just 'ORSet
    TRga       _            -> Just 'RGA
    TStructLww _            -> Nothing
    TVersionVector          -> Nothing

mkGuideType :: RonType -> TH.TypeQ
mkGuideType typ = case typ of
    TAtom      _            -> view
    TOpaque    _            -> view
    TORSet     item
        | isObjectType item -> wrap ''ObjectORSet item
        | otherwise         -> wrap ''ORSet       item
    TRga       item         -> wrap ''RGA         item
    TStructLww _            -> view
    TVersionVector          -> view
  where
    view = mkViewType typ
    wrap w item = [t| $(conT w) $(mkGuideType item) |]

data Field' = Field'
    { field'Name     :: Text
    , field'Optional :: Bool
    , field'RonName  :: UUID
    , field'Type     :: RonType
    , field'Var      :: TH.Name
    }

mkReplicatedStructLww :: StructLww -> TH.DecsQ
mkReplicatedStructLww struct = do
    fields <- for (Map.assocs structFields) $ \(field'Name, field) -> let
        Field{fieldType, fieldAnnotations} = field
        FieldAnnotations{faOptional} = fieldAnnotations
        field'Optional = faOptional
        field'Type = fieldType
        in
        case UUID.mkName . BSC.pack $ Text.unpack field'Name of
            Just field'RonName -> do
                field'Var <- TH.newName $ Text.unpack field'Name
                pure Field'{..}
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show field'Name
    dataType <- mkDataType
    [instanceReplicated] <- mkInstanceReplicated
    [instanceReplicatedAsObject] <- mkInstanceReplicatedAsObject fields
    accessors <- fold <$> traverse mkAccessors fields
    pure $
        dataType : instanceReplicated : instanceReplicatedAsObject : accessors
  where

    StructLww{structName, structFields, structAnnotations} = struct

    StructAnnotations{saHaskellDeriving, saHaskellFieldPrefix} =
        structAnnotations

    name = mkNameT structName

    structT = conT name

    objectT = [t| Object $structT |]

    mkDataType = dataD (TH.cxt []) name [] Nothing
        [recC name
            [ TH.varBangType (mkNameT $ mkHaskellFieldName fieldName) $
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                if faOptional then [t| Maybe $vt |] else vt
            | (fieldName, Field fieldType FieldAnnotations{faOptional}) <-
                Map.assocs structFields
            , let vt = mkViewType fieldType
            ]]
        [TH.derivClause Nothing . map (conT . mkNameT) $
            toList saHaskellDeriving]

    mkInstanceReplicated = [d|
        instance Replicated $structT where
            encoding = objectEncoding
        |]

    mkInstanceReplicatedAsObject fields = do
        obj   <- TH.newName "obj"
        frame <- TH.newName "frame"
        ops   <- TH.newName "ops"
        let fieldsToUnpack =
                [ bindS var [|
                    LWW.viewField
                        $(liftData field'RonName) $(varE ops) $(varE frame)
                    |]
                | Field'{field'Type, field'Var, field'RonName} <- fields
                , let
                    fieldP = varP field'Var
                    var = maybe fieldP (\w -> conP w [fieldP]) $
                        fieldWrapperC field'Type
                ]
        let getObjectImpl = doE
                $   letS [valD' frame [| objectFrame $(varE obj) |]]
                :   bindS (varP ops) [| getObjectStateChunk $(varE obj) |]
                :   fieldsToUnpack
                ++  [noBindS [| pure $consE |]]
        [d| instance ReplicatedAsObject $structT where
                objectOpType = lwwType
                newObject $consP = LWW.newFrame $fieldsToPack
                getObject $(varP obj) = fmapL ($(lift errCtx) ++) $getObjectImpl
            |]
      where
        fieldsToPack = listE
            [ [| ($(liftData field'RonName), I $var) |]
            | Field'{field'Type, field'Var, field'RonName} <- fields
            , let
                fieldVarE = varE field'Var
                var = case fieldWrapperC field'Type of
                    Nothing  -> fieldVarE
                    Just con -> [| $(conE con) $fieldVarE |]
            ]
        errCtx = "getObject @" ++ Text.unpack structName ++ ":\n"
        consE = recConE name
            [ pure (fieldName, VarE field'Var)
            | Field'{field'Name, field'Var} <- fields
            , let fieldName = mkNameT $ mkHaskellFieldName field'Name
            ]
        consP = conP name [varP field'Var | Field'{field'Var} <- fields]

    mkHaskellFieldName = (saHaskellFieldPrefix <>)

    mkAccessors field' = do
        a <- varT <$> TH.newName "a"
        m <- varT <$> TH.newName "m"
        sequenceA
            $   (guard field'Optional *>
                [ sigD has [t|
                    (MonadError String $m, MonadState $objectT $m) => $m Bool
                    |]
                , valD' has [| LWW.hasField $(liftData field'RonName) |]
                ])
            ++  if isObjectType field'Type then
                    [ sigD zoom [t|
                        MonadError String $m
                        => StateT (Object $(mkGuideType field'Type)) $m $a
                        -> StateT $objectT $m $a
                        |]
                    , valD' zoom [| LWW.zoomField $(liftData field'RonName) |]
                    ]
                else
                    [ sigD assign [t|
                        (Clock $m, MonadError String $m, MonadState $objectT $m)
                        => $fieldViewType -> $m ()
                        |]
                    , valD' assign
                        [| LWW.assignField $(liftData field'RonName) . I |]
                    , sigD get [t|
                        (MonadError String $m, MonadState $objectT $m)
                        => $m $fieldViewType
                        |]
                    , valD' get [| LWW.getField $(liftData field'RonName) |]
                    ]
      where
        Field'{field'Name, field'Optional, field'RonName, field'Type} = field'
        fieldViewType = mkViewType field'Type
        assign = mkNameT $ "assign_" <> mkHaskellFieldName field'Name
        get    = mkNameT $ "get_"    <> mkHaskellFieldName field'Name
        has    = mkNameT $ "has_"    <> mkHaskellFieldName field'Name
        zoom   = mkNameT $ "zoom_"   <> mkHaskellFieldName field'Name

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: RonType -> TH.TypeQ
mkViewType = \case
    TAtom atom -> case atom of
        TAInteger -> [t| Int64 |]
        TAString  -> [t| Text |]
    TOpaque Opaque{opaqueAnnotations} -> let
        OpaqueAnnotations{oaHaskellType} = opaqueAnnotations
        in case oaHaskellType of
            Just name -> conT $ mkNameT name
            Nothing   -> fail "Opaque type must define a Haskell type"
    TORSet item -> wrapList item
    TRga   item -> wrapList item
    TStructLww StructLww{structName} -> conT $ mkNameT structName
    TVersionVector -> [t| VersionVector |]
  where
    wrapList a = [t| [$(mkViewType a)] |]

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

isObjectType :: RonType -> Bool
isObjectType = \case
    TAtom      _                      -> False
    TOpaque    Opaque{opaqueIsObject} -> opaqueIsObject
    TORSet     _                      -> True
    TRga       _                      -> True
    TStructLww _                      -> True
    TVersionVector                    -> True
