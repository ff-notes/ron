{-# OPTIONS -Wno-orphans #-}

module Orphans () where

import           RON.Prelude

import           Hedgehog (MonadTest, liftTest)

import           RON.Event.Simulation (NetworkSimT, ReplicaSimT)

instance MonadTest m => MonadTest (NetworkSimT m) where
    liftTest = lift . liftTest

instance MonadTest m => MonadTest (ReplicaSimT m) where
    liftTest = lift . liftTest
