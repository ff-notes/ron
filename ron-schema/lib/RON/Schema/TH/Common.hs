{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH.Common (
    isObjectType,
    let1S,
    liftText,
    mkGuideType,
    mkNameT,
    mkViewType,
    valD,
    valDP,
) where

import           RON.Prelude

import qualified Data.Text as Text
import           Language.Haskell.TH (conT, normalB, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftString)

import           RON.Data.ORSet (ORSet (..), ObjectORSet (..))
import           RON.Data.RGA (RGA (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Schema as X

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

mkViewType :: HasCallStack => RonType -> TH.TypeQ
mkViewType = \case
    TAtom atom -> case atom of
        TAInteger -> [t| Int64 |]
        TAString  -> [t| Text |]
    TComposite t -> case t of
        TEnum   Enum{enumName} -> conT $ mkNameT enumName
        TOption u              -> [t| Maybe $(mkViewType u) |]
    TObject t -> case t of
        TORSet     item                  -> wrapList item
        TRga       item                  -> wrapList item
        TStructLww StructLww{structName} -> conT $ mkNameT structName
        TVersionVector                   -> [t| VersionVector |]
    TOpaque Opaque{opaqueName, opaqueAnnotations} -> let
        OpaqueAnnotations{oaHaskellType} = opaqueAnnotations
        in conT $ mkNameT $ fromMaybe opaqueName oaHaskellType
  where
    wrapList a = [t| [$(mkViewType a)] |]

isObjectType :: RonType -> Bool
isObjectType = \case
    TAtom      _                      -> False
    TComposite _                      -> False
    TObject    _                      -> True
    TOpaque    Opaque{opaqueIsObject} -> opaqueIsObject

valD :: TH.PatQ -> TH.ExpQ -> TH.DecQ
valD pat body = TH.valD pat (normalB body) []

valDP :: TH.Name -> TH.ExpQ -> TH.DecQ
valDP = valD . varP

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

liftText :: Text -> TH.ExpQ
liftText t = [| Text.pack $(liftString $ Text.unpack t) |]

let1S :: TH.PatQ -> TH.ExpQ -> TH.StmtQ
let1S pat exp = TH.letS [valD pat exp]
