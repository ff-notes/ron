{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Experimental (
  AsAtoms (..),
  Replicated (..),
  ReplicatedObject (..),
  ) where

import           RON.Prelude

import           RON.Error (MonadE)
import           RON.Types (Atom, OpenFrame, UUID)

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

class AsAtoms a where
  toAtoms   :: a -> [Atom]
  fromAtoms :: MonadE m => [Atom] -> m a

instance AsAtoms [Atom] where
  toAtoms   = id
  fromAtoms = pure
