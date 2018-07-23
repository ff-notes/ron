module RON.Typed.Text (parse) where

import           RON.Internal.Prelude

import           RON.Text (parseFrame)
-- import           RON.Typed (Replicated (..))

parse ::
    -- Replicated a =>
    ByteStringL -> Either String a
parse = parseFrame >=> error "TODO: fromFrame"
