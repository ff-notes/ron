{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Data.Time (Day, day) where

import           Data.Time (Day, fromGregorian, toGregorian)

import           RON.Data (Replicated (..), ReplicatedAsPayload (..),
                           payloadEncoding)
import           RON.Schema (OpaqueAnnotations (..), RonType, def, opaqueAtoms)
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
        _ -> Left "bad Day"

day :: RonType
day = opaqueAtoms def{oaHaskellType = Just "Day"}
