{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           Hedgehog (failure, forAll, property, success, (===))
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified Gen

import           Swarm.DB.Replica (get, newTextReplica)
import           Swarm.RON.Status (Status (Status), code, notOpen)

main = $defaultMainGenerator

prop_uninitialized_replica = property $ do
    replica <- liftIO newTextReplica
    key <- forAll Gen.uuid
    got <- liftIO $ get key replica
    case got of
        Left status@Status{code}
            | code == notOpen -> success
            | otherwise       -> status === Status notOpen ""
        Right _ -> failure
