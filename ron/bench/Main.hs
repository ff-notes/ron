{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
import           Control.Monad (void)
import           Criterion (bench, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (timeLimit)

import           RON.Text (parseWireFrames, serializeWireFrames)
import           RON.Types (Atom, Op (..), RawOp (..), UUID, WireChunk (Raw),
                            WireReducedChunk)
import qualified RON.UUID as UUID

deriving instance NFData Atom
deriving instance NFData Op
deriving instance NFData RawOp
deriving instance NFData UUID
deriving instance NFData WireChunk
deriving instance NFData WireReducedChunk

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
