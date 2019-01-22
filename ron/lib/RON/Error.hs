{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module RON.Error (
    MonadE,
    errorContext,
    liftMaybe,
    throwErrorString,
) where

import           Data.String (IsString, fromString)

type MonadE = MonadError String

errorContext :: MonadE m => String -> m a -> m a
errorContext ctx action = action `catchError` (throwError . ((ctx ++ ": ") ++))

liftMaybe :: MonadE m => String -> Maybe a -> m a
liftMaybe msg = maybe (throwErrorString msg) pure

throwErrorString :: (MonadError e m, IsString e) => String -> m a
throwErrorString msg = throwError $ fromString msg
