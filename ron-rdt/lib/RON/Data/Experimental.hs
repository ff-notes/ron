{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.Experimental (Replicated (..), ReplicatedObject (..)) where

import           RON.Error (MonadE)
import           RON.Types (UUID)
import           RON.Types.Experimental (OpenFrame)

class Replicated a where
  -- | UUID of the type
  replicatedTypeId :: UUID

  stateFromFrame :: OpenFrame -> a

class (Replicated (Rep a)) => ReplicatedObject a where
  -- | RON representation type
  type Rep a

  view :: MonadE m => Rep a -> m a
