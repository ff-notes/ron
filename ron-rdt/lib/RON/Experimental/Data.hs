{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Experimental.Data (
  AsAtom (..),
  AsAtoms (..),
  BaseType (..),
  ReplicatedObject (..),
  baseTypeId,
  ) where

import           RON.Prelude

import           RON.Error (MonadE, throwErrorText)
import           RON.Event (ReplicaClock)
import           RON.Store.Class (MonadStore)
import           RON.Types (Atom (AString, AUuid), ObjectRef (..), OpenFrame,
                            UUID)
import           RON.Types.Experimental (Ref (..))
import qualified RON.UUID as UUID

-- | Basic RON type.
data BaseType = LWW | Set

baseTypeId :: BaseType -> UUID
baseTypeId LWW = $(UUID.liftName "lww")
baseTypeId Set = $(UUID.liftName "set")

-- | Any type that may be encoded as a RON object in whole.
class ReplicatedObject a where

  -- | RON representation type
  baseType :: BaseType
  baseType = Set

  encodeObject ::
    (MonadStore m, ReplicaClock m) =>
    -- | Object id
    UUID ->
    a ->
    m ()

  decodeObject ::
    MonadE m =>
    -- | Object id
    UUID ->
    -- | Object ops
    OpenFrame ->
    m a

class AsAtom a where
  toAtom   :: a -> Atom
  fromAtom :: MonadE m => Atom -> m a

instance AsAtom Atom where
  toAtom = id
  fromAtom = pure

instance AsAtom UUID where
  toAtom = AUuid

  fromAtom = \case
    AUuid u -> pure u
    a       -> throwErrorText $ "Expected UUID atom, got " <> show a

instance AsAtom Text where
  toAtom = AString

  fromAtom = \case
    AString t -> pure t
    a         -> throwErrorText $ "Expected string atom, got " <> show a

class AsAtoms a where
  toAtoms   :: a -> [Atom]
  fromAtoms :: MonadE m => [Atom] -> m a

instance AsAtoms [Atom] where
  toAtoms   = id
  fromAtoms = pure

instance AsAtoms Atom where
  toAtoms a = [a]

  fromAtoms = \case
    [a] -> pure a
    as  -> throwErrorText $ "Expected 1 atom, got " <> show (length as)

instance AsAtoms UUID where
  toAtoms u = [AUuid u]

  fromAtoms as = do
    a <- fromAtoms as
    case a of
      AUuid u -> pure u
      _       -> throwErrorText $ "Expected UUID, got " <> show a

instance AsAtoms Text where
  toAtoms a = [toAtom a]
  fromAtoms = fromAtoms @Atom >=> fromAtom @Text

instance AsAtoms (ObjectRef a) where
  toAtoms (ObjectRef uuid) = toAtoms uuid
  fromAtoms = fmap ObjectRef . fromAtoms

instance (AsAtom head, AsAtoms tail) => AsAtoms (head, tail) where
  toAtoms (head, tail) = toAtom head : toAtoms tail

  fromAtoms = \case
    [] -> throwErrorText "Expected some atoms, got none"
    head : tail -> (,) <$> fromAtom head <*> fromAtoms tail

instance AsAtoms (Ref a) where
  toAtoms Ref{object, path} = AUuid object : path

  fromAtoms = \case
    []                  -> throwErrorText "Expected some atoms, got none"
    AUuid object : path -> pure Ref{object, path}
    a            : _    -> throwErrorText $ "Expected UUID, got " <> show a
