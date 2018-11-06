{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH
    ( mkReplicated
    ) where

import           Prelude hiding (read)
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
                             FieldAnnotations (..), OpaqueAnnotations (..),
                             RonType (..), Schema, StructAnnotations (..),
                             StructLww (..), TAtom (..), TComposite (..),
                             TObject (..), TOpaque (..))
import           RON.Types (Object (..), UUID)
import qualified RON.UUID as UUID

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

-- | Type-directing newtype
fieldWrapperC :: RonType -> Maybe TH.Name
fieldWrapperC typ = case typ of
    TAtom                   _ -> Nothing
    TComposite              _ -> Nothing
    TObject                 t -> case t of
        TORSet              a
            | isObjectType  a -> Just 'ObjectORSet
            | otherwise       -> Just 'ORSet
        TRga                _ -> Just 'RGA
        TStructLww          _ -> Nothing
        TVersionVector        -> Nothing
    TOpaque                 _ -> Nothing

mkGuideType :: RonType -> TH.TypeQ
mkGuideType typ = case typ of
    TAtom                   _ -> view
    TComposite              _ -> view
    TObject                 t -> case t of
        TORSet              a
            | isObjectType  a -> wrap ''ObjectORSet a
            | otherwise       -> wrap ''ORSet       a
        TRga                a -> wrap ''RGA         a
        TStructLww          _ -> view
        TVersionVector        -> view
    TOpaque                 _ -> view
  where
    view = mkViewType typ
    wrap w item = [t| $(conT w) $(mkGuideType item) |]

data Field' = Field'
    { field'Name     :: Text
    , field'RonName  :: UUID
    , field'Type     :: RonType
    , field'Var      :: TH.Name
    }

mkReplicatedStructLww :: StructLww -> TH.DecsQ
mkReplicatedStructLww struct = do
    fields <- for (Map.assocs structFields) $ \(field'Name, field) -> let
        Field{fieldType} = field
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
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) viewType
            | (fieldName, Field fieldType FieldAnnotations) <-
                Map.assocs structFields
            , let viewType = mkViewType fieldType
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
                newObject $consP = LWW.newObject $fieldsToPack
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
        let assignF =
                [ sigD assign [t|
                    (Clock $m, MonadError String $m, MonadState $objectT $m)
                    => $fieldViewType -> $m ()
                    |]
                , valD' assign
                    [| LWW.assignField $(liftData field'RonName) . $guide |]
                ]
            readF =
                [ sigD read [t|
                    (MonadError String $m, MonadState $objectT $m)
                    => $m $fieldViewType
                    |]
                , valD' read
                    [| $unguide <$> LWW.readField $(liftData field'RonName) |]
                ]
            zoomF =
                [ sigD zoom [t|
                    MonadError String $m
                    => StateT (Object $(mkGuideType field'Type)) $m $a
                    -> StateT $objectT $m $a
                    |]
                , valD' zoom [| LWW.zoomField $(liftData field'RonName) |]
                ]
        sequenceA $ assignF ++ readF ++ zoomF
      where
        Field'{field'Name, field'RonName, field'Type} = field'
        fieldViewType = mkViewType field'Type
        assign = mkNameT $ mkHaskellFieldName field'Name <> "_assign"
        read   = mkNameT $ mkHaskellFieldName field'Name <> "_read"
        zoom   = mkNameT $ mkHaskellFieldName field'Name <> "_zoom"
        guidedX = case fieldWrapperC field'Type of
            Just w  -> conP w [x]
            Nothing -> x
          where
            x = varP $ TH.mkName "x"
        unguide = [| \ $guidedX -> x |]
        guide = case fieldWrapperC field'Type of
            Just w  -> conE w
            Nothing -> [| id |]

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: RonType -> TH.TypeQ
mkViewType = \case
    TAtom atom -> case atom of
        TAInteger -> [t| Int64 |]
        TAString  -> [t| Text |]
    TComposite t -> case t of
        TOption u -> [t| Maybe $(mkViewType u) |]
    TObject t -> case t of
        TORSet     item                  -> wrapList item
        TRga       item                  -> wrapList item
        TStructLww StructLww{structName} -> conT $ mkNameT structName
        TVersionVector                   -> [t| VersionVector |]
    TOpaque Opaque{opaqueAnnotations} -> let
        OpaqueAnnotations{oaHaskellType} = opaqueAnnotations
        in case oaHaskellType of
            Just name -> conT $ mkNameT name
            Nothing   -> fail "Opaque type must define a Haskell type"
  where
    wrapList a = [t| [$(mkViewType a)] |]

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

isObjectType :: RonType -> Bool
isObjectType = \case
    TAtom      _                      -> False
    TComposite _                      -> False
    TObject    _                      -> True
    TOpaque    Opaque{opaqueIsObject} -> opaqueIsObject
