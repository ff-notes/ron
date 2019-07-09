module RON.Data.ORSet.Map (
    Map (..),
) where

newtype Map k v = Map [(k, v)]
