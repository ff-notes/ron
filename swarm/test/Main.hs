{-# OPTIONS -Wwarn=missing-signatures #-}
{-# OPTIONS -Wwarn=unused-imports #-}
{-# OPTIONS -Wwarn=unused-matches #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           RON.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (runResourceT)
import           Hedgehog (evalEither, forAll, property, test, (===))
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import qualified Gen

import           Swarm.DB.Replica (createBranch, createObject, createReplica,
                                   getObject, newTextReplica, open)
import           Swarm.RON.Status (Status (Status), notOpen, ok)

main = $defaultMainGenerator

-- prop_uninitialized_replica = property $ do
--     object <- forAll Gen.uuid
--     typ    <- forAll Gen.uuid
--     yarn   <- forAll Gen.word64'
--     got    <- liftIO . runResourceT $ do
--         replica <- newTextReplica
--         liftIO $ getObject (#object object) (#type typ) (#yarn yarn) replica
--     Left (Status notOpen "") === got

prop_put_get = property $ do
    typ  <- forAll Gen.uuid
    yarn <- forAll Gen.word64'
    hoist runResourceT $ do
        replica <- lift newTextReplica
        -- pure ()

        replicaCreated <- liftIO $ createReplica replica
        Status ok "" === replicaCreated

        replicaOpened <- liftIO $ open replica
        Status ok "" === replicaOpened

        branchCreated <- liftIO $ createBranch (#yarn yarn) replica
        Status ok "" === branchCreated

        objectCreated <-
            liftIO $ createObject (#type typ) (#yarn yarn) replica
        Right () === objectCreated

        objectGot <- liftIO $
            getObject (error "objectId") (#type typ) (#yarn yarn) replica
        value <- evalEither objectGot
        "value" === value
