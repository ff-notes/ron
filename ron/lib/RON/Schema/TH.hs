{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
import           Language.Haskell.TH (Exp (VarE), appE, appT, arrowT, bindS,
                                      conE, conP, conT, dataD, doE, forallT,
                                      funD, instanceD, letS, listE, listT,
                                      noBindS, recC, recConE, sigD, tupE, varE,
                                      varP, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData, liftString)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk, objectEncoding)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (AsORSet (..), AsObjectMap (..))
import           RON.Data.RGA (AsRga (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Event (Clock)
import           RON.Schema (Alias (..), AliasAnnotations (..),
                             Declaration (..), Field (..), RonType (..), Schema,
                             StructAnnotations (..), StructLww (..), TAtom (..))
import           RON.Types (Object (..))
import qualified RON.UUID as UUID

mkReplicated :: Schema -> TH.DecsQ
mkReplicated = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DStructLww s -> mkReplicatedStructLww s

fieldWrapperC :: RonType -> Maybe TH.Name
fieldWrapperC typ = case typ of
    TAlias     _            -> Nothing
    TAtom      _            -> Nothing
    TAtomTuple _            -> Nothing
    TORSet     item
        | isObjectType item -> Just 'AsObjectMap
        | otherwise         -> Just 'AsORSet
    TRga       _            -> Just 'AsRga
    TStructLww _            -> Nothing
    TVersionVector          -> Nothing

mkGuideType :: RonType -> TH.TypeQ
mkGuideType typ = case typ of
    TAlias     _            -> view
    TAtom      _            -> view
    TAtomTuple _            -> view
    TORSet     item
        | isObjectType item -> wrap item ''AsObjectMap
        | otherwise         -> wrap item ''AsORSet
    TRga       item         -> wrap item ''AsRga
    TStructLww _            -> view
    TVersionVector          -> view
  where
    view = mkViewType typ
    wrap item w = conT w `appT` mkGuideType item

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
            [ TH.varBangType (mkNameT fieldName) $
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                mkViewType fieldType
            | (fieldName, Field fieldType _) <- Map.assocs structFields
            ]]
        [TH.derivClause Nothing . map (conT . mkNameT) $
            toList saHaskellDeriving]

    mkInstanceReplicated = instanceD (TH.cxt [])
        (conT ''Replicated `appT` conT name)
        [valD' 'encoding [| objectEncoding |]]

    mkInstanceReplicatedAsObject fields = do
        let fieldsToPack = listE
                [ tupE [liftData fieldNameUuid, [| I |] `appE` var]
                | (fieldNameUuid, _, Field fieldType _, fieldVar) <-
                    fields
                , let var = maybe id (appE . conE) (fieldWrapperC fieldType) $
                        varE fieldVar
                ]
        obj   <- TH.newName "obj";   let objE   = varE obj
        frame <- TH.newName "frame"; let frameE = varE frame
        ops   <- TH.newName "ops";   let opsE   = varE ops
        let fieldsToUnpack =
                [ bindS var $
                    [| LWW.getField |] `appE` liftData fieldNameUuid
                    `appE` opsE `appE` frameE
                | (fieldNameUuid, _, Field fieldType _, fieldVar) <-
                    fields
                , let
                    fieldP = varP fieldVar
                    var = maybe fieldP (\w -> conP w [fieldP]) $
                        fieldWrapperC fieldType
                ]
        instanceD (TH.cxt [])
            (conT ''ReplicatedAsObject `appT` conT name)
            [ valD' 'objectOpType [| lwwType |]
            , funD 'newObject
                [clause'
                    [conP name [varP fieldVar | (_, _, _, fieldVar) <- fields]]
                    $ [| LWW.newFrame |] `appE` fieldsToPack]
            , funD 'getObject
                [clause' [varP obj] $
                    appE
                        [| fmapL $ (++)
                            $(liftString $
                                "getObject @" ++ structName' ++ ":\n") |]
                    $ doE
                    $ letS [valD' frame $ [| objectFrame |] `appE` objE]
                    : bindS (varP ops) ([| getObjectStateChunk |] `appE` objE)
                    : fieldsToUnpack
                    ++ [noBindS $ [| pure |] `appE` cons]]
            ]
      where
        structName' = Text.unpack structName
        cons = recConE
            name
            [ pure (fieldName, VarE fieldVar)
            | (_, field, _, fieldVar) <- fields, let fieldName = mkNameT field
            ]

    mkAccessors (nameUuid, fname, Field typ _, _)
        | isObjectType typ =
            [ sigD with $
                forallT [] (TH.cxt [[t| MonadError String |] `appT` m]) $
                arrowT `appT`
                    ([t| StateT |] `appT`
                        ([t| Object |] `appT` mkGuideType typ) `appT`
                        m `appT`
                        unitT) `appT`
                    ([t| StateT |] `appT`
                        ([t| Object |] `appT` conT name) `appT`
                        m `appT`
                        unitT)
            , valD' with [| LWW.withField $(liftData nameUuid) |]
            ]
        | otherwise =
            [ sigD set $
                forallT []
                    (TH.cxt
                        [ [t| Clock |] `appT` m
                        , [t| MonadError String |] `appT` m
                        , [t| MonadState |]
                            `appT` ([t| Object |] `appT` conT name)
                            `appT` m
                        ]) $
                arrowT `appT` mkViewType typ `appT` (m `appT` unitT)
            , valD' set [| LWW.writeField $(liftData nameUuid) . I |]
            ]
      where
        set  = mkNameT $ "set_"  <> fname
        with = mkNameT $ "with_" <> fname
        m = varT (TH.mkName "m")
        unitT = TH.tupleT 0

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: RonType -> TH.TypeQ
mkViewType = \case
    TAlias Alias{aliasAnnotations = AliasAnnotations{..}, ..} ->
        case aaHaskellType of
            Nothing     -> mkViewType aliasType
            Just hsType -> conT $ mkNameT hsType
    TAtom atom -> case atom of
        TAInteger -> conT ''Int64
        TAString  -> conT ''Text
    TAtomTuple _ -> undefined
    TORSet item -> wrapList item
    TRga   item -> wrapList item
    TStructLww StructLww{..} -> conT $ mkNameT structName
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
    TAtomTuple _   -> False
    TORSet     _   -> True
    TRga       _   -> True
    TStructLww _   -> True
    TVersionVector -> True
