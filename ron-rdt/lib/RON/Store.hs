{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Store (
  MonadStore (..),
  newObject,
  readObject,
) where

import           RON.Prelude

import           Data.List (stripPrefix)
import           RON.Data.Experimental (Rep, ReplicatedObject, replicatedTypeId,
                                        stateFromFrame, view)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Store.Class (MonadStore (..))
import           RON.Types (Op (..))
import           RON.Types.Experimental (Patch (..), Ref (..))

newObject ::
  forall a m. (MonadStore m, ReplicatedObject a, ReplicaClock m) => m (Ref a)
newObject = do
  objectId <- getEventUuid
  let typeId = replicatedTypeId @(Rep a)
  let initOp = Op{opId = objectId, refId = typeId, payload = []}
  appendPatch $ Patch objectId $ initOp :| []
  pure $ Ref objectId []

-- | Nothing if object doesn't exist in the replica.
readObject ::
  (MonadE m, MonadStore m, ReplicatedObject a, Typeable a) =>
  Ref a -> m (Maybe a)
readObject object@(Ref objectId path) =
  errorContext ("readObject " <> show object) $ do
    ops <- fold <$> loadObjectLog objectId mempty
    case ops of
      [] -> pure Nothing
      _ ->
        fmap Just $
        view objectId $
        stateFromFrame objectId $
        sortOn
          opId
          [ op{payload = payload'}
          | op@Op{payload} <- ops
          , Just payload' <- [stripPrefix path payload]
          ]
