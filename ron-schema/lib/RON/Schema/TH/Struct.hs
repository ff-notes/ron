{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Reduce duplication" -}
module RON.Schema.TH.Struct
  ( mkReplicatedStructLww,
    mkReplicatedStructSet
    )
where

import Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Haskell.TH
  ( bindS,
    conT,
    doE,
    listE,
    newName,
    noBindS,
    recC,
    recConE,
    recP,
    sigD,
    varE,
    varP,
    varT
    )
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (liftData)
import RON.Data
  ( MonadObjectState,
    ObjectStateT,
    Replicated (encoding),
    ReplicatedAsObject (Rep, newObject, readObject),
    getObjectStateChunk,
    objectEncoding
    )
import RON.Data.LWW (LwwRep)
import qualified RON.Data.LWW as LWW
import RON.Data.ORSet (ORSetRep)
import qualified RON.Data.ORSet as ORSet
import RON.Error (MonadE, errorContext)
import RON.Event (ReplicaClock)
import RON.Prelude
import RON.Schema as X
import RON.Schema.TH.Common
  ( fieldExp',
    fieldPat',
    liftText,
    mkGuideType,
    mkNameT,
    newNameT,
    valDP
    )
import RON.Types (Object (Object), UUID)
import qualified RON.UUID as UUID
import RON.Util (Instance (Instance))

type instance XField Equipped = XFieldEquipped

data XFieldEquipped
  = XFieldEquipped
      { haskellName :: Text,
        ronName :: UUID
        }

equipStruct :: Struct e Resolved -> Struct e Equipped
equipStruct Struct {name, fields, annotations} = Struct
  { name,
    fields = Map.mapWithKey (equipField annotations) fields,
    annotations
    }

mkReplicatedStructLww :: StructLww Resolved -> TH.DecsQ
mkReplicatedStructLww structResolved = do
  dataType <- mkDataType struct
  [instanceReplicated] <- mkInstanceReplicated type'
  [instanceReplicatedAO] <- mkInstanceReplicatedAOLww struct
  accessors <- fold <$> traverse (mkAccessorsLww name') fields
  pure $ dataType : instanceReplicated : instanceReplicatedAO : accessors
  where
    struct@Struct {name, fields} = equipStruct structResolved
    name' = mkNameT name
    type' = conT name'

mkReplicatedStructSet :: StructSet Resolved -> TH.DecsQ
mkReplicatedStructSet structResolved = do
  dataType <- mkDataType struct
  [instanceReplicated] <- mkInstanceReplicated type'
  [instanceReplicatedAO] <- mkInstanceReplicatedAOSet struct
  accessors <- fold <$> traverse (mkAccessorsSet name') fields
  pure $ dataType : instanceReplicated : instanceReplicatedAO : accessors
  where
    struct@Struct {name, fields} = equipStruct structResolved
    name' = mkNameT name
    type' = conT name'

equipField :: StructAnnotations -> Text -> Field Resolved -> Field Equipped
equipField structAnnotations schemaName Field {..} =
  case UUID.mkName $ Text.encodeUtf8 schemaName of
    Just ronName -> Field {ext = XFieldEquipped {haskellName, ronName}, ..}
    Nothing ->
      error $ "Field name is not representable in RON: " ++ show schemaName
  where
    haskellName = mkHaskellFieldName structAnnotations schemaName

varBangType' :: Text -> TH.TypeQ -> TH.VarBangTypeQ
varBangType' name =
  TH.varBangType (mkNameT name)
    . TH.bangType (TH.bang TH.noSourceUnpackedness TH.sourceStrict)

mkDataType :: Struct encoding Equipped -> TH.DecQ
mkDataType Struct {name, fields} =
  TH.dataD
    (TH.cxt [])
    name'
    []
    Nothing
    [ recC name'
        [ varBangType' haskellName [t|Maybe $(mkGuideType ronType)|]
          | Field {ronType, ext = XFieldEquipped {haskellName}} <- toList fields
          ]
      ]
    []
  where
    name' = mkNameT name

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated type' =
  [d|
    instance Replicated $type' where

      encoding = objectEncoding
    |]

mkInstanceReplicatedAOLww :: StructLww Equipped -> TH.DecsQ
mkInstanceReplicatedAOLww Struct {name, fields} = do
  ops <- newName "ops"
  vars <- traverse (newNameT . haskellName . ext) fields
  let packFields =
        listE
          [ [|($ronName', Instance <$> $(varE var))|]
            | Field {ext = XFieldEquipped {ronName}} <- toList fields,
              let ronName' = liftData ronName
            | var <- toList vars
            ]
      unpackFields =
        [ bindS (varP var) [|LWW.viewField $ronName' $(varE ops)|]
          | Field {ext = XFieldEquipped {ronName}} <- toList fields,
            let ronName' = liftData ronName
          | var <- toList vars
          ]
  let consE =
        recConE name'
          [ fieldExp' haskellName $ varE var
            | Field {ext = XFieldEquipped {haskellName}} <- toList fields
            | var <- toList vars
            ]
      consP =
        recP name'
          [ fieldPat' haskellName $ varP var
            | Field {ext = XFieldEquipped {haskellName}} <- toList fields
            | var <- toList vars
            ]
  let readObjectImpl =
        doE
          $ bindS (varP ops) [|getObjectStateChunk|]
          : unpackFields
          ++ [noBindS [|pure $consE|]]
  [d|
    instance ReplicatedAsObject $type' where

      type Rep $type' = LwwRep

      newObject $consP = Object <$> LWW.newStruct $packFields

      readObject = errorContext $(liftText errCtx) $readObjectImpl
    |]
  where
    name' = mkNameT name
    type' = conT name'
    errCtx = "readObject @" <> name

mkInstanceReplicatedAOSet :: StructSet Equipped -> TH.DecsQ
mkInstanceReplicatedAOSet Struct {name, fields} = do
  ops <- newName "ops"
  vars <- traverse (newNameT . haskellName . ext) fields
  let packFields =
        listE
          [ [|($ronName', fmap Instance $(varE var))|]
            | Field {ext = XFieldEquipped {ronName}} <- toList fields,
              let ronName' = liftData ronName
            | var <- toList vars
            ]
      unpackFields =
        [ bindS
            (varP var)
            [|
              errorContext $(liftText haskellName)
                $ $(orSetViewField mergeStrategy) $ronName' $(varE ops)
              |]
          | Field {annotations = FieldAnnotations {mergeStrategy}, ext} <-
              toList fields,
            let XFieldEquipped {haskellName, ronName} = ext
                ronName' = liftData ronName
          | var <- toList vars
          ]
  let consE =
        recConE name'
          [ fieldExp' haskellName $ varE var
            | Field {ext = XFieldEquipped {haskellName}} <- toList fields
            | var <- toList vars
            ]
      consP =
        recP name'
          [ fieldPat' haskellName $ varP var
            | Field {ext = XFieldEquipped {haskellName}} <- toList fields
            | var <- toList vars
            ]
  let readObjectImpl =
        doE
          $ bindS (varP ops) [|getObjectStateChunk|]
          : unpackFields
          ++ [noBindS [|pure $consE|]]
  [d|
    instance ReplicatedAsObject $type' where

      type Rep $type' = ORSetRep

      newObject $consP = Object <$> ORSet.newStruct $packFields

      readObject = errorContext $(liftText errCtx) $readObjectImpl
    |]
  where
    name' = mkNameT name
    type' = conT name'
    errCtx = "readObject @" <> name

mkHaskellFieldName :: StructAnnotations -> Text -> Text
mkHaskellFieldName annotations base = prefix <> base'
  where
    StructAnnotations
      { haskellFieldPrefix = prefix,
        haskellFieldCaseTransform = caseTransform
        } =
        annotations
    base' = case caseTransform of
      Nothing -> base
      Just TitleCase -> case Text.uncons base of
        Nothing -> base
        Just (b, baseTail) -> Text.cons (toTitle b) baseTail

mkAccessorsLww :: TH.Name -> Field Equipped -> TH.DecsQ
mkAccessorsLww name' field = do
  a <- varT <$> newName "a"
  m <- varT <$> newName "m"
  let setF =
        [ sigD set
            [t|
              (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
              => Maybe $guideType
              -> $m ()
              |],
          valDP set [|LWW.assignField $ronName'|]
          ]
      readF =
        [ sigD read
            [t|
              (MonadE $m, MonadObjectState $type' $m) => $m (Maybe $guideType)
              |],
          valDP read [|LWW.readField $ronName'|]
          ]
      zoomF =
        [ sigD zoom
            [t|
              MonadE $m
              => ObjectStateT $guideType $m $a
              -> ObjectStateT $type' $m $a
              |],
          valDP zoom [|LWW.zoomField $ronName'|]
          ]
  sequenceA $ setF ++ readF ++ zoomF
  where
    Field {ronType, ext = XFieldEquipped {haskellName, ronName}} = field
    ronName' = liftData ronName
    type' = conT name'
    guideType = mkGuideType ronType
    set  = mkNameT $ haskellName <> "_set"
    read = mkNameT $ haskellName <> "_read"
    zoom = mkNameT $ haskellName <> "_zoom"

mkAccessorsSet :: TH.Name -> Field Equipped -> TH.DecsQ
mkAccessorsSet name' field = do
  a <- varT <$> newName "a"
  m <- varT <$> newName "m"
  let setF =
        [ sigD set
            [t|
              (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
              => $guideType -> $m ()
              |],
          valDP set [|ORSet.assignField $ronName' . Just|]
          ]
      clearF =
        [ sigD clear
            [t|
              (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m) => $m ()
              |],
          valDP clear
            [|ORSet.assignField $ronName' (Nothing :: Maybe $guideType)|]
          ]
      getF = do
        TObject _ <- [ronType]
        [ sigD getName
            [t|
              (MonadE $m, MonadObjectState $type' $m)
              => $m (Maybe (Object $guideType))
              |],
          valDP getName [|ORSet.getFieldObject $ronName'|]
          ]
      readF =
        [ sigD read
            [t|
              (MonadE $m, MonadObjectState $type' $m) => $m (Maybe $guideType)
              |],
          valDP read
            [|
              do
                chunk <- getObjectStateChunk
                $(orSetViewField mergeStrategy) $ronName' chunk
              |]
          ]
      zoomF = do
        TObject _ <- [ronType]
        [ sigD zoom
            [t|
              (MonadE $m, ReplicaClock $m)
              => ObjectStateT $guideType $m $a -> ObjectStateT $type' $m $a
              |],
          valDP zoom [|ORSet.zoomFieldObject $ronName'|]
          ]
  sequenceA $ setF ++ clearF ++ getF ++ readF ++ zoomF
  where
    Field {ronType, annotations = FieldAnnotations {mergeStrategy}, ext} = field
    XFieldEquipped {haskellName, ronName} = ext
    ronName' = liftData ronName
    type' = conT name'
    guideType = mkGuideType ronType
    set     = mkNameT $ haskellName <> "_set"
    clear   = mkNameT $ haskellName <> "_clear"
    getName = mkNameT $ haskellName <> "_get"
    read    = mkNameT $ haskellName <> "_read"
    zoom    = mkNameT $ haskellName <> "_zoom"

orSetViewField :: Maybe MergeStrategy -> TH.ExpQ
orSetViewField = varE . \case
  Nothing  -> 'ORSet.viewField
  Just LWW -> 'ORSet.viewFieldLWW
  Just Max -> 'ORSet.viewFieldMax
  Just Min -> 'ORSet.viewFieldMin
  Just Set -> 'ORSet.viewFieldSet
