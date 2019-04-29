{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           Hedgehog (failure, forAll, property, success, (===))
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified Gen
import           RON.Data.LWW (lwwType)

import           Swarm.DB.Replica (newTextReplica, receive)
import           Swarm.RON.Status (Status (Status), code, notOpen)

main = $defaultMainGenerator

prop_uninitialized_replica = property $ do
    replica <- liftIO newTextReplica
    key <- forAll Gen.uuid
    liftIO (receive key lwwType replica) >>= \case
        Left status@Status{code}
            | code == notOpen -> success
            | otherwise        -> status === Status notOpen ""
        Right _ -> failure
