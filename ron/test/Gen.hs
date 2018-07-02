{-# LANGUAGE NamedFieldPuns #-}

module Gen where

import           Data.Bits ((.&.))
import           Data.Fixed (Fixed (MkFixed))
import           Data.Time (TimeOfDay (..), UTCTime (..), fromGregorian,
                            timeOfDayToTime)
import           Data.Word (Word64)
import           Hedgehog (MonadGen)
import           Hedgehog.Gen (integral, list, word64)
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

frame :: MonadGen gen => Int -> gen Frame
frame size = list (Range.exponential 0 size) op

frames :: MonadGen gen => Int -> Int -> gen [Frame]
frames frameCount opCount =
    list (Range.exponential 0 frameCount) (frame opCount)

-- | Event time with year 2010—2350
eventTime :: MonadGen gen => gen UTCTime
eventTime = do
    y  <- integral $ Range.constant 2010 2350
    m  <- integral $ Range.constant 1 12
    d  <- integral $ Range.constant 1 31
    hh <- integral $ Range.constant 0 23
    mm <- integral $ Range.constant 0 59
    ss <- fmap (MkFixed . (*100000)) $ integral $ Range.constant 0 599999999
    pure UTCTime
        { utctDay     = fromGregorian y m d
        , utctDayTime = timeOfDayToTime $ TimeOfDay hh mm ss
        }
