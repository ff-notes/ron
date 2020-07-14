module Fork (fork) where

import           Control.Concurrent (ThreadId, forkFinally, myThreadId)
import           Control.Exception.Safe (SomeException, throwTo, Exception)

fork :: IO () -> IO ()
fork thread =
  do
    parent <- myThreadId
    _ <- forkFinally thread $ either (rethrow parent) pure
    pure ()
  where
    rethrow parent exception = do
      child <- myThreadId
      throwTo parent Rethrowing{parent, child, exception}

data Rethrowing = Rethrowing
  { parent    :: ThreadId
  , child     :: ThreadId
  , exception :: SomeException
  }
  deriving anyclass (Exception)
  deriving stock (Show)
