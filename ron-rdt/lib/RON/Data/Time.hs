{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Day' instances
module RON.Data.Time (Day) where

import           RON.Prelude

import           Data.Time (Day, fromGregorian, toGregorian)

import           RON.Data (Replicated, fromPayload, toPayload)
import           RON.Types (Atom (AInteger))

instance Replicated Day where
    toPayload
        = (\(y, m, d) ->
            map AInteger [fromIntegral y, fromIntegral m, fromIntegral d])
        . toGregorian

    fromPayload = \case
        [AInteger y, AInteger m, AInteger d] -> pure $
            fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
        _ -> throwError "bad Day"
