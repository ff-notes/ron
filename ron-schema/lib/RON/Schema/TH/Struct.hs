{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Schema.TH.Struct (mkReplicatedStructLww) where

import           RON.Prelude

import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (Exp (VarE), bindS, conP, conT, doE, listE,
                                      noBindS, recC, recConE, sigD, varE, varP,
                                      varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (MonadObjectState, ObjectStateT, Rep, Replicated,
                           ReplicatedAsObject, ReplicatedBoundedSemilattice,
                           encoding, getObject, getObjectStateChunk, newObject,
                           objectEncoding, objectRconcat, rconcat)
import           RON.Data.LWW (LwwRep)
import qualified RON.Data.LWW as LWW
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock)
import           RON.Schema as X
import           RON.Schema.TH.Common (liftText, mkGuideType, mkNameT, valDP)
import           RON.Types (Object (Object), UUID)
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

data Field' = Field'
    { haskellName :: Text
    , ronName     :: UUID
    , ronType     :: RonType
    , var         :: TH.Name
    }

mkReplicatedStructLww :: StructLww 'Resolved -> TH.DecsQ
mkReplicatedStructLww StructLww{name, fields, annotations} = do
    fields' <- for (Map.assocs fields) $ \(haskellName, Field{ronType}) ->
        case UUID.mkName . BSC.pack $ Text.unpack haskellName of
            Just ronName -> do
                var <- TH.newName $ Text.unpack haskellName
                pure Field'{haskellName, ronName, ronType, var}
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show haskellName
    dataType <- mkDataType name' fields annotations
    [instanceReplicated]   <- mkInstanceReplicated   type'
    [instanceReplicatedBS] <- mkInstanceReplicatedBS type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAO name fields' annotations
    accessors <- fold <$> traverse (mkAccessors type' annotations) fields'
    pure
        $ dataType
        : instanceReplicated : instanceReplicatedBS : instanceReplicatedAO
        : accessors
  where
    name' = mkNameT name
    type' = conT    name'

mkDataType
    :: TH.Name -> Map Text (Field Resolved) -> StructAnnotations -> TH.DecQ
mkDataType name fields annotations = TH.dataD (TH.cxt []) name [] Nothing
    [recC name
        [ TH.varBangType (mkNameT $ mkHaskellFieldName annotations fieldName) $
            TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
            mkGuideType ronType
        | (fieldName, Field ronType) <- Map.assocs fields
        ]]
    []

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated structType = [d|
    instance Replicated $structType where
        encoding = objectEncoding
    |]

mkInstanceReplicatedBS :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicatedBS type' = [d|
    instance ReplicatedBoundedSemilattice $type' where
        rconcat = objectRconcat
    |]

mkInstanceReplicatedAO
    :: Text -> [Field'] -> StructAnnotations -> TH.DecsQ
mkInstanceReplicatedAO name fields annotations = do
    ops <- TH.newName "ops"
    let fieldsToUnpack =
            [ bindS (varP var)
                [| LWW.viewField $(liftData ronName) $(varE ops) |]
            | Field'{var, ronName} <- fields
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   fieldsToUnpack
            ++  [noBindS [| pure $consE |]]
    [d| instance ReplicatedAsObject $structType where
            type Rep $structType = LwwRep
            newObject $consP = Object <$> LWW.newObject $fieldsToPack
            getObject =
                errorContext $(liftText errCtx) $getObjectImpl
        |]
  where
    name' = mkNameT name
    structType = conT name'
    fieldsToPack = listE
        [ [| ($(liftData ronName), Instance $(varE var)) |]
        | Field'{var, ronName} <- fields
        ]
    errCtx = "getObject @" <> name <> ":\n"
    consE = recConE name'
        [ pure (fieldName, VarE var)
        | Field'{haskellName, var} <- fields
        , let fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
        ]
    consP = conP name' [varP var | Field'{var} <- fields]

mkHaskellFieldName :: StructAnnotations -> Text -> Text
mkHaskellFieldName annotations base = prefix <> base' where
    StructAnnotations
            { haskellFieldPrefix        = prefix
            , haskellFieldCaseTransform = caseTransform
            }
        = annotations
    base' = case caseTransform of
        Nothing        -> base
        Just TitleCase -> case Text.uncons base of
            Nothing            -> base
            Just (b, baseTail) -> Text.cons (toTitle b) baseTail

mkAccessors :: TH.TypeQ -> StructAnnotations -> Field' -> TH.DecsQ
mkAccessors structType annotations field' = do
    a <- varT <$> TH.newName "a"
    m <- varT <$> TH.newName "m"
    let assignF =
            [ sigD assign [t|
                (ReplicaClock $m, MonadE $m, MonadObjectState $structType $m)
                => $fieldGuideType -> $m ()
                |]
            , valDP assign [| LWW.assignField $(liftData ronName) |]
            ]
        readF =
            [ sigD read [t|
                (MonadE $m, MonadObjectState $structType $m)
                => $m $fieldGuideType
                |]
            , valDP read [| LWW.readField $(liftData ronName) |]
            ]
        zoomF =
            [ sigD zoom [t|
                MonadE $m
                => ObjectStateT $(mkGuideType ronType) $m $a
                -> ObjectStateT $structType                  $m $a
                |]
            , valDP zoom [| LWW.zoomField $(liftData ronName) |]
            ]
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field'{haskellName, ronName, ronType} = field'
    fieldGuideType = mkGuideType ronType
    assign = mkNameT $ mkHaskellFieldName annotations haskellName <> "_assign"
    read   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_read"
    zoom   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_zoom"
