{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Day' instances
module RON.Data.Time (Day) where

import           RON.Prelude

import           Data.Time (Day, fromGregorian, toGregorian)

import           RON.Data (Replicated (..), ReplicatedAsPayload (..),
                           payloadEncoding)
import           RON.Types (Atom (..))

instance Replicated Day where encoding = payloadEncoding

instance ReplicatedAsPayload Day where
    toPayload
        = (\(y, m, d) ->
            map AInteger [fromIntegral y, fromIntegral m, fromIntegral d])
        . toGregorian

    fromPayload = \case
        [AInteger y, AInteger m, AInteger d] -> pure $
            fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
        _ -> throwError "bad Day"
