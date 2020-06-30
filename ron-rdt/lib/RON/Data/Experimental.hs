{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Experimental (
  AsAtom (..),
  AsAtoms (..),
  Replicated (..),
  ReplicatedObject (..),
  ) where

import           RON.Prelude

import           RON.Error (MonadE, throwErrorText)
import           RON.Types (Atom (AString, AUuid), ObjectRef (..), OpenFrame,
                            UUID)

class Replicated a where

  -- | UUID of the type
  replicatedTypeId :: UUID

  stateFromFrame :: UUID -> OpenFrame -> a

class (Replicated (Rep a)) => ReplicatedObject a where

  -- | RON representation type
  type Rep a

  view ::
    MonadE m =>
      -- | Object id
      UUID ->
      Rep a ->
      m a

class AsAtom a where
  toAtom   :: a -> Atom
  fromAtom :: MonadE m => Atom -> m a

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
