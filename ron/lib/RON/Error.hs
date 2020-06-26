{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    tryIO,
) where

import           RON.Prelude

import           Control.Exception (SomeException, try)
import           Data.String (IsString, fromString)
import qualified Data.Text as Text
import           GHC.Stack (callStack, getCallStack, prettySrcLoc)
import qualified Text.Show

data Error = Error{description :: Text, reasons :: [Error]}
  deriving (Eq)

instance Show Error where
  show =
    unlines . go
    where
      go Error{description, reasons} =
        Text.unpack description : foldMap (map ("  " ++) . go) reasons

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

tryIO :: (MonadE m, MonadIO m, HasCallStack) => IO a -> m a
tryIO action = do
  e <- liftIO $ try action
  case e of
    Right a  -> pure a
    Left exc -> do
      let
        description =
          case getCallStack callStack of
            [] -> "tryIO"
            (f, loc) : _ -> Text.pack $ f ++ ", called at " ++ prettySrcLoc loc
      throwError
        Error{description, reasons = [fromString (show (exc :: SomeException))]}
