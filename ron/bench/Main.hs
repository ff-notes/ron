import           Internal.Prelude

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Criterion (bench, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (timeLimit)

import           RON.Text (parseFrames, serializeFrames)
import           RON.Types (Op (..), UUID (..))

main :: IO ()
main = do
    void . evaluate $ force serialized
    defaultMainWith
        defaultConfig{timeLimit = 1}
        [bench (show n) $ nf parseFrames batch | (n, batch) <- serialized]
  where
    u = UUID 0 0
    op = Op{typ = u, object = u, event = u, location = u}
    frame n = replicate n op

    serialized =
        [ (n :: Int, serializeFrames $ replicate 100 $ frame n)
        | i <- [1 .. 10], let n = 100 * i
        ]
