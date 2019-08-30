{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RON.Prelude.Writer
  ( module X
    )
where

import Control.Monad.Trans.Writer.CPS as X (WriterT, execWriterT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.Class as X (MonadWriter, listen, pass, tell, writer)
import Prelude (Monad, Monoid)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where

  listen = CPS.listen

  pass = CPS.pass

  tell = CPS.tell

  writer = CPS.writer
