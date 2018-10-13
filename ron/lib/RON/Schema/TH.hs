{-# LANGUAGE LambdaCase #-}
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
import           RON.Schema (Declaration (..), Field (..), Opaque (..),
                             OpaqueAnnotations (..), RonType (..), Schema,
                             StructAnnotations (..), StructLww (..), TAtom (..))
import           RON.Types (Object (..))
import qualified RON.UUID as UUID

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

fieldWrapperC :: RonType -> Maybe TH.Name
fieldWrapperC typ = case typ of
    TAtom      _            -> Nothing
    TOpaque    _            -> Nothing
    TOption    _            -> Nothing
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
    TOption    t            -> [t| Maybe $(mkGuideType t) |]
    TORSet     item
        | isObjectType item -> wrap ''ObjectORSet item
        | otherwise         -> wrap ''ORSet       item
    TRga       item         -> wrap ''RGA         item
    TStructLww _            -> view
    TVersionVector          -> view
  where
    view = mkViewType typ
    wrap w item = [t| $(conT w) $(mkGuideType item) |]

mkReplicatedStructLww :: StructLww -> TH.DecsQ
mkReplicatedStructLww StructLww{..} = do
    fields <- for (Map.assocs structFields) $ \(fieldName, fieldType) ->
        case UUID.mkName . BSC.pack $ Text.unpack fieldName of
            Just fieldNameUuid -> do
                fieldVar <- TH.newName $ Text.unpack fieldName
                pure (fieldNameUuid, fieldName, fieldType, fieldVar)
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show fieldName
    dataType <- mkData
    [instanceReplicated] <- mkInstanceReplicated
    [instanceReplicatedAsObject] <- mkInstanceReplicatedAsObject fields
    accessors <- fold <$> traverse mkAccessors fields
    pure $
        dataType : instanceReplicated : instanceReplicatedAsObject : accessors
  where

    StructAnnotations{..} = structAnnotations
    name = mkNameT structName

    mkData = dataD (TH.cxt []) name [] Nothing
        [recC name
            [ TH.varBangType (mkNameT $ mkHaskellFieldName fieldName) $
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                mkViewType fieldType
            | (fieldName, Field fieldType _) <- Map.assocs structFields
            ]]
        [TH.derivClause Nothing . map (conT . mkNameT) $
            toList saHaskellDeriving]

    mkInstanceReplicated = [d|
        instance Replicated $(conT name) where
            encoding = objectEncoding
        |]

    mkInstanceReplicatedAsObject fields = do
        obj   <- TH.newName "obj"
        frame <- TH.newName "frame"
        ops   <- TH.newName "ops"
        let fieldsToUnpack =
                [ bindS var [|
                    LWW.viewField
                        $(liftData fieldNameUuid) $(varE ops) $(varE frame)
                    |]
                | (fieldNameUuid, _, Field fieldType _, fieldVar) <- fields
                , let
                    fieldP = varP fieldVar
                    var = maybe fieldP (\w -> conP w [fieldP]) $
                        fieldWrapperC fieldType
                ]
        let getObjectImpl = doE
                $   letS [valD' frame [| objectFrame $(varE obj) |]]
                :   bindS (varP ops) [| getObjectStateChunk $(varE obj) |]
                :   fieldsToUnpack
                ++  [noBindS [| pure $consE |]]
        [d| instance ReplicatedAsObject $(conT name) where
                objectOpType = lwwType
                newObject $consP = LWW.newFrame $fieldsToPack
                getObject $(varP obj) = fmapL ($(lift errCtx) ++) $getObjectImpl
            |]
      where
        fieldsToPack = listE
            [ [| ($(liftData fieldNameUuid), I $var) |]
            | (fieldNameUuid, _, Field fieldType _, fieldVar) <- fields
            , let
                fieldVarE = varE fieldVar
                var = case fieldWrapperC fieldType of
                    Nothing  -> fieldVarE
                    Just con -> [| $(conE con) $fieldVarE |]
            ]
        errCtx = "getObject @" ++ Text.unpack structName ++ ":\n"
        consE = recConE name
            [ pure (fieldName, VarE fieldVar)
            | (_, field, _, fieldVar) <- fields
            , let fieldName = mkNameT $ mkHaskellFieldName field
            ]
        consP = conP name [varP fieldVar | (_, _, _, fieldVar) <- fields]

    mkHaskellFieldName = (saHaskellFieldPrefix <>)

    mkAccessors (nameUuid, fname, Field typ _, _) = do
        a <- varT <$> TH.newName "a"
        m <- varT <$> TH.newName "m"
        sequenceA $
            if isObjectType typ then
                [ sigD zoom
                    [t| MonadError String $m
                        => StateT (Object $(mkGuideType typ)) $m $a
                        -> StateT (Object $(conT name))       $m $a |]
                , valD' zoom [| LWW.zoomField $(liftData nameUuid) |]
                ]
            else
                [ sigD assign
                    [t| ( Clock $m
                        , MonadError String $m
                        , MonadState (Object $(conT name)) $m
                        )
                        => $(mkViewType typ) -> $m () |]
                , valD' assign [| LWW.assignField $(liftData nameUuid) . I |]
                ]
      where
        assign = mkNameT $ "assign_" <> mkHaskellFieldName fname
        zoom   = mkNameT $ "zoom_"   <> mkHaskellFieldName fname

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: RonType -> TH.TypeQ
mkViewType = \case
    TAtom atom -> case atom of
        TAInteger -> [t| Int64 |]
        TAString  -> [t| Text |]
    TOpaque Opaque{..} -> let
        OpaqueAnnotations{..} = opaqueAnnotations
        in case oaHaskellType of
            Just name -> conT $ mkNameT name
            Nothing   -> fail "Opaque type must define a Haskell type"
    TOption t -> [t| Maybe $(mkViewType t) |]
    TORSet item -> wrapList item
    TRga   item -> wrapList item
    TStructLww StructLww{..} -> conT $ mkNameT structName
    TVersionVector -> [t| VersionVector |]
  where
    wrapList a = [t| [$(mkViewType a)] |]

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

isObjectType :: RonType -> Bool
isObjectType = \case
    TAtom      _          -> False
    TOpaque    Opaque{..} -> opaqueIsObject
    TOption    t          -> isObjectType t
    TORSet     _          -> True
    TRga       _          -> True
    TStructLww _          -> True
    TVersionVector        -> True
