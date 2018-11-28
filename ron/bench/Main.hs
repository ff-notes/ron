{-# LANGUAGE NamedFieldPuns #-}

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (void)
import           Criterion (bench, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (timeLimit)

import           RON.Text (parseWireFrames, serializeWireFrames)
import           RON.Types (Op (..), RawOp (..), WireChunk (Raw))
import qualified RON.UUID as UUID

main :: IO ()
main = do
    void . evaluate $ force serialized
    defaultMainWith
        defaultConfig{timeLimit = 1}
        [bench (show n) $ nf parseWireFrames batch | (n, batch) <- serialized]
  where
    rawop = RawOp{opType = UUID.zero, opObject = UUID.zero, op}
    op = Op{opEvent = UUID.zero, opRef = UUID.zero, opPayload = []}
    frame n = replicate n $ Raw rawop

    serialized =
        [ (n :: Int, serializeWireFrames $ replicate 100 $ frame n)
        | i <- [1 .. 10], let n = 100 * i
        ]
