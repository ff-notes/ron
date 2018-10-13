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
import           Language.Haskell.TH (Exp (VarE), appE, bindS, conE, conP, conT,
                                      dataD, doE, funD, instanceD, letS, listE,
                                      noBindS, recC, recConE, sigD, tupE, varE,
                                      varP, varT)
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
    sequence
        $ mkData
        : mkInstanceReplicated
        : mkInstanceReplicatedAsObject fields
        : concatMap mkAccessors fields
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

    mkInstanceReplicated = instanceD (TH.cxt []) [t| Replicated $(conT name) |]
        [valD' 'encoding [| objectEncoding |]]

    mkInstanceReplicatedAsObject fields = do
        let fieldsToPack = listE
                [ tupE [liftData fieldNameUuid, [| I $var |]]
                | (fieldNameUuid, _, Field fieldType _, fieldVar) <- fields
                , let var = maybe id (appE . conE) (fieldWrapperC fieldType) $
                        varE fieldVar
                ]
        obj   <- TH.newName "obj";   let objE   = varE obj
        frame <- TH.newName "frame"; let frameE = varE frame
        ops   <- TH.newName "ops";   let opsE   = varE ops
        let fieldsToUnpack =
                [ bindS var
                    [| LWW.getField $(liftData fieldNameUuid) $opsE $frameE |]
                | (fieldNameUuid, _, Field fieldType _, fieldVar) <- fields
                , let
                    fieldP = varP fieldVar
                    var = maybe fieldP (\w -> conP w [fieldP]) $
                        fieldWrapperC fieldType
                ]
        instanceD (TH.cxt []) [t| ReplicatedAsObject $(conT name) |]
            [ valD' 'objectOpType [| lwwType |]
            , funD 'newObject
                [clause'
                    [conP name [varP fieldVar | (_, _, _, fieldVar) <- fields]]
                    [| LWW.newFrame $fieldsToPack |]]
            , funD 'getObject
                [clause' [varP obj] $
                    appE [| fmapL $ (++) $(lift errCtx) |]
                    $ doE
                    $ letS [valD' frame [| objectFrame $objE |]]
                    : bindS (varP ops) [| getObjectStateChunk $objE |]
                    : fieldsToUnpack
                    ++ [noBindS [| pure $cons |]]]
            ]
      where
        errCtx = "getObject @" ++ Text.unpack structName ++ ":\n"
        cons = recConE
            name
            [ pure (fieldName, VarE fieldVar)
            | (_, field, _, fieldVar) <- fields
            , let fieldName = mkNameT $ mkHaskellFieldName field
            ]

    mkHaskellFieldName = (saHaskellFieldPrefix <>)

    mkAccessors (nameUuid, fname, Field typ _, _)
        | isObjectType typ =
            [ sigD zoom
                [t| MonadError String $m
                    => StateT (Object $(mkGuideType typ)) $m $a
                    -> StateT (Object $(conT name))       $m $a |]
            , valD' zoom [| LWW.withField $(liftData nameUuid) |]
            ]
        | otherwise =
            [ sigD assign
                [t| ( Clock $m
                    , MonadError String $m
                    , MonadState (Object $(conT name)) $m
                    )
                    => $(mkViewType typ) -> $m () |]
            , valD' assign [| LWW.writeField $(liftData nameUuid) . I |]
            ]
      where
        assign = mkNameT $ "assign_" <> mkHaskellFieldName fname
        zoom   = mkNameT $ "zoom_"   <> mkHaskellFieldName fname
        a = varT (TH.mkName "a")
        m = varT (TH.mkName "m")

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: RonType -> TH.TypeQ
mkViewType = \case
    TAtom atom -> case atom of
        TAInteger -> conT ''Int64
        TAString  -> conT ''Text
    TOpaque Opaque{..} -> let
        OpaqueAnnotations{..} = opaqueAnnotations
        in case oaHaskellType of
            Just name -> conT $ mkNameT name
            Nothing   -> fail "Opaque type must define a Haskell type"
    TOption t -> [t| Maybe $(mkViewType t) |]
    TORSet item -> wrapList item
    TRga   item -> wrapList item
    TStructLww StructLww{..} -> conT $ mkNameT structName
    TVersionVector -> conT ''VersionVector
  where
    wrapList a = [t| [$(mkViewType a)] |]

valD' :: TH.Name -> TH.ExpQ -> TH.DecQ
valD' name body = TH.valD (varP name) (TH.normalB body) []

clause' :: [TH.PatQ] -> TH.ExpQ -> TH.ClauseQ
clause' pat body = TH.clause pat (TH.normalB body) []

isObjectType :: RonType -> Bool
isObjectType = \case
    TAtom      _          -> False
    TOpaque    Opaque{..} -> opaqueIsObject
    TOption    t          -> isObjectType t
    TORSet     _          -> True
    TRga       _          -> True
    TStructLww _          -> True
    TVersionVector        -> True
