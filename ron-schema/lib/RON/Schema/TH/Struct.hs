{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Reduce duplication" -}
module RON.Schema.TH.Struct (
  mkReplicatedStructLww,
  mkReplicatedStructSet,
) where

import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH (conT, newName, recC, sigD, varE, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (Rep, Replicated (fromPayload, toPayload),
                           getObjectStateChunk)
import           RON.Data.LWW (LwwRep)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (ORSetRep)
import qualified RON.Data.ORSet as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Prelude
import           RON.Schema as X
import           RON.Schema.TH.Common (mkFieldType, mkGuideType, mkNameT, valDP)
import           RON.Types (ObjectRef, StateFrame, UUID)
import qualified RON.UUID as UUID

type instance XField Equipped = XFieldEquipped

data XFieldEquipped = XFieldEquipped
  { haskellName :: Text
  , ronName     :: UUID
  }

equipStruct :: Struct e Resolved -> Struct e Equipped
equipStruct Struct{name, fields, annotations} =
  Struct
    { name
    , fields = Map.mapWithKey (equipField annotations) fields
    , annotations
    }

mkReplicatedStructLww :: StructLww Resolved -> TH.DecsQ
mkReplicatedStructLww structResolved =
    do  dataType             <- mkDataTypeLww struct
        [instanceReplicated] <- mkInstanceReplicated type'
        [instanceRep]        <- mkInstanceRepLww struct
        accessors            <- fold <$> traverse (mkAccessorsLww name') fields
        pure $ dataType : instanceReplicated : instanceRep : accessors
    where
        struct@Struct{name, fields} = equipStruct structResolved
        name' = mkNameT name
        type' = conT name'

mkReplicatedStructSet :: StructSet Resolved -> TH.DecsQ
mkReplicatedStructSet structResolved =
    do  dataType             <- mkDataTypeSet struct
        [instanceReplicated] <- mkInstanceReplicated type'
        [instanceRep]        <- mkInstanceRepSet struct
        accessors            <- fold <$> traverse (mkAccessorsSet name') fields
        pure $ dataType : instanceReplicated : instanceRep : accessors
    where
        struct@Struct{name, fields} = equipStruct structResolved
        name' = mkNameT name
        type' = conT name'

equipField :: StructAnnotations -> Text -> Field Resolved -> Field Equipped
equipField structAnnotations schemaName Field{..} =
  case UUID.mkName $ Text.encodeUtf8 schemaName of
    Just ronName -> Field{ext = XFieldEquipped{haskellName, ronName}, ..}
    Nothing ->
      error $ "Field name is not representable in RON: " ++ show schemaName
  where
    haskellName = mkHaskellFieldName structAnnotations schemaName

varBangType' :: Text -> TH.TypeQ -> TH.VarBangTypeQ
varBangType' name =
  TH.varBangType (mkNameT name)
    . TH.bangType (TH.bang TH.noSourceUnpackedness TH.sourceStrict)

mkDataTypeLww :: StructLww Equipped -> TH.DecQ
mkDataTypeLww Struct{name, fields} =
  TH.dataD
    (TH.cxt [])
    name'
    []
    Nothing
    [ recC
        name'
        [ varBangType' haskellName [t|Maybe $(mkGuideType ronType)|]
          | Field{ronType, ext = XFieldEquipped{haskellName}} <- toList fields
        ]
    ]
    []
  where
    name' = mkNameT name

mkDataTypeSet :: StructSet Equipped -> TH.DecQ
mkDataTypeSet Struct{name, fields} =
  TH.dataD
    (TH.cxt [])
    name'
    []
    Nothing
    [ recC
        name'
        [ varBangType' haskellName (mkFieldType ronType mergeStrategy)
        | Field{ronType, mergeStrategy, ext} <- toList fields
        , let XFieldEquipped{haskellName} = ext
        ]
    ]
    []
  where
    name' = mkNameT name

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated type' =
  [d|
    instance Replicated $type' where
      toPayload   = undefined
      fromPayload = undefined
  |]

mkInstanceRepLww :: StructLww Equipped -> TH.DecsQ
mkInstanceRepLww Struct{name} =
  [d|type instance Rep $type' = LwwRep|]
  where
    name' = mkNameT name
    type' = conT name'

mkInstanceRepSet :: StructSet Equipped -> TH.DecsQ
mkInstanceRepSet Struct{name} =
  [d|type instance Rep $type' = ORSetRep|]
  where
    name' = mkNameT name
    type' = conT name'

mkHaskellFieldName :: StructAnnotations -> Text -> Text
mkHaskellFieldName annotations base = prefix <> base' where
    StructAnnotations{
        haskellFieldPrefix = prefix,
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
    m <- varT <$> newName "m"
    let setF = [
            sigD
                set
                [t| (ReplicaClock $m, MonadE $m, MonadState StateFrame $m) =>
                    Maybe $guideType -> ObjectRef $type' -> $m ()
                    |],
            valDP set [|LWW.assignField $ronName'|]
            ]
        readF = [
            sigD
                read
                [t| (MonadE $m, MonadState StateFrame $m) =>
                    ObjectRef $type' -> $m (Maybe $guideType)
                    |],
            valDP read [|LWW.readField $ronName'|]
            ]
    sequenceA $ setF ++ readF

    where
        Field{ronType, ext = XFieldEquipped{haskellName, ronName}} = field
        ronName' = liftData ronName
        type' = conT name'
        guideType = mkGuideType ronType
        set = mkNameT $ haskellName <> "_set"
        read = mkNameT $ haskellName <> "_read"

mkAccessorsSet :: TH.Name -> Field Equipped -> TH.DecsQ
mkAccessorsSet name' field = do
    m <- varT <$> newName "m"
    let addF = [
            sigD
                add
                [t| (ReplicaClock $m, MonadE $m, MonadState StateFrame $m) =>
                    $guideType -> ObjectRef $type' -> $m ()
                    |],
            valDP add [|ORSet.addFieldValue $ronName'|]
            ]
    let setF = [
            sigD
                set
                [t| (ReplicaClock $m, MonadE $m, MonadState StateFrame $m) =>
                    $guideType -> ObjectRef $type' -> $m ()
                    |],
            valDP set [|ORSet.assignField $ronName' . Just|]
            ]
    let clearF = [
            sigD
                clear
                [t| (ReplicaClock $m, MonadE $m, MonadState StateFrame $m) =>
                    ObjectRef $type' -> $m ()
                    |],
            valDP
                clear
                [|ORSet.assignField $ronName' (Nothing :: Maybe $guideType)|]
            ]
    let getF = [
            sigD
                get_
                [t| (MonadE $m, MonadState StateFrame $m) =>
                    ObjectRef $type' -> $m $fieldType
                    |],
            valDP
                get_
                [|  \obj -> do
                        chunk <- getObjectStateChunk obj
                        $(orSetViewField mergeStrategy) $ronName' chunk
                    |]
            ]
    let removeF = do
            guard $ mergeStrategy == Set
            [   sigD
                    remove
                    [t| (   MonadE $m,
                            MonadState StateFrame $m,
                            ReplicaClock $m
                            ) =>
                        $guideType -> ObjectRef $type' -> $m ()
                        |],
                valDP remove [|ORSet.removeFieldValue $ronName'|],
                sigD
                    removeIf
                    [t| (   MonadE $m,
                            MonadState StateFrame $m,
                            ReplicaClock $m
                            ) =>
                        ($guideType -> $m Bool) -> ObjectRef $type' -> $m ()
                        |],
                valDP removeIf [|ORSet.removeFieldValueIf $ronName'|]
                ]
    sequenceA $ addF ++ setF ++ clearF ++ getF ++ removeF
    where
        Field{mergeStrategy, ronType, ext} = field
        XFieldEquipped{haskellName, ronName} = ext
        ronName'  = liftData ronName
        type'     = conT name'
        fieldType = mkFieldType ronType mergeStrategy
        guideType = mkGuideType ronType
        add       = mkNameT $ haskellName <> "_add"
        clear     = mkNameT $ haskellName <> "_clear"
        get_      = mkNameT $ haskellName <> "_get"
        remove    = mkNameT $ haskellName <> "_remove"
        removeIf  = mkNameT $ haskellName <> "_removeIf"
        set       = mkNameT $ haskellName <> "_set"

orSetViewField :: MergeStrategy -> TH.ExpQ
orSetViewField = varE . \case
  Monoid -> 'ORSet.viewFieldObjectMonoid
  LWW    -> 'ORSet.viewFieldLWW
  Max    -> 'ORSet.viewFieldMax
  Min    -> 'ORSet.viewFieldMin
  Set    -> 'ORSet.viewFieldSet
