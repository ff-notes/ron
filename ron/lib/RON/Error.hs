{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module RON.Error (
    Error (..),
    MonadE,
    correct,
    errorContext,
    liftEither,
    liftEitherString,
    liftMaybe,
    throwErrorString,
    throwErrorText,
) where

import           RON.Prelude

import           Data.String (IsString, fromString)

data Error = Error Text [Error]
    deriving (Eq, Show)
{-  TODO(2019-08-09, cblp)
    data Error = Error
        { context :: [Text]
        , desc    :: Text
        , reasons :: [Error]
        }
-}

instance Exception Error

instance IsString Error where
    fromString s = Error (fromString s) []

type MonadE = MonadError Error

errorContext :: MonadE m => Text -> m a -> m a
errorContext ctx action = action `catchError` \e -> throwError $ Error ctx [e]

liftMaybe :: MonadE m => Text -> Maybe a -> m a
liftMaybe msg = maybe (throwErrorText msg) pure

liftEitherString :: (MonadError e m, IsString e) => Either String a -> m a
liftEitherString = either throwErrorString pure

throwErrorText :: MonadE m => Text -> m a
throwErrorText msg = throwError $ Error msg []

throwErrorString :: (MonadError e m, IsString e) => String -> m a
throwErrorString = throwError . fromString

correct :: MonadError e m => a -> m a -> m a
correct def action =
    action
    `catchError` \_e ->
        -- TODO(2019-08-06, cblp) $logWarnSH e
        pure def
