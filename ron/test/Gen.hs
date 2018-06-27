module Gen where

import           Data.Bits ((.&.))
import           Data.Word (Word64)
import           Hedgehog (MonadGen)
import           Hedgehog.Gen (list, word64)
import qualified Hedgehog.Range as Range

import           RON.Types (Frame, Op (..), UUID (..))

word64' :: MonadGen gen => gen Word64
word64' = word64 Range.linearBounded

word60 :: MonadGen gen => gen Word64
word60 = (.&. 0x0FFFFFFFFFFFFFFF) <$> word64'

uuid :: MonadGen gen => gen UUID
uuid = UUID <$> word64' <*> word64'

op :: MonadGen gen => gen Op
op = Op <$> uuid <*> uuid <*> uuid <*> uuid

frame :: MonadGen gen => gen Frame
frame = list (Range.exponential 0 100) op
