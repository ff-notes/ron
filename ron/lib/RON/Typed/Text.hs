module RON.Typed.Text (serialize) where

import           RON.Internal.Prelude

import qualified RON.Text as Text
import           RON.Typed (Object, Replicated, objectToStateChunk)
import           RON.Types (Chunk (State))

serialize :: Replicated a => Object a -> Either String ByteStringL
serialize obj = do
    chunk <- objectToStateChunk obj
    pure $ Text.serializeFrame [State chunk]
