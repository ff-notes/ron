{-# OPTIONS -Wno-orphans #-}

module Orphans () where

import           Hedgehog (MonadTest, liftTest)

import           RON.Event.Simulation (ReplicaSimT)

instance MonadTest m => MonadTest (ReplicaSimT m) where
    liftTest = lift . liftTest
