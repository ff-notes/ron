{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Criterion (bench, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (timeLimit)

import           RON.Text (parseWireFrames, serializeWireFrames)
import           RON.Types (Atom, ClosedOp (..), Op (..), UUID,
                            WireChunk (Closed), WireReducedChunk)
import qualified RON.UUID as UUID

deriving instance NFData Atom
deriving instance NFData ClosedOp
deriving instance NFData Op
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
    closedOp = ClosedOp{opType = UUID.zero, opObject = UUID.zero, op}
    op = Op{opId = UUID.zero, refId = UUID.zero, payload = []}
    frame n = replicate n $ Closed closedOp

    serialized =
        [ (n :: Int, serializeWireFrames $ replicate 100 $ frame n)
        | i <- [1 .. 10], let n = 100 * i
        ]
