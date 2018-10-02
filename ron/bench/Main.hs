{-# LANGUAGE NamedFieldPuns #-}

import           RON.Internal.Prelude

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Criterion (bench, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (timeLimit)

import           RON.Text (parseFrames, serializeFrames)
import           RON.Types (Chunk (Raw), Op (..), RawOp (..))
import qualified RON.UUID as UUID

main :: IO ()
main = do
    void . evaluate $ force serialized
    defaultMainWith
        defaultConfig{timeLimit = 1}
        [bench (show n) $ nf parseFrames batch | (n, batch) <- serialized]
  where
    rawop = RawOp{opType = UUID.zero, opObject = UUID.zero, op}
    op = Op{opEvent = UUID.zero, opRef = UUID.zero, opPayload = []}
    frame n = replicate n $ Raw rawop

    serialized =
        [ (n :: Int, serializeFrames $ replicate 100 $ frame n)
        | i <- [1 .. 10], let n = 100 * i
        ]
