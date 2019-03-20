{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified RON.UUID as UUID
import           Swarm.DB.Replica (TextReplica, get, new)

main :: IO ()
main = do
    replica <- new @TextReplica
    got <- get UUID.zero replica
    case got of
        Left errorStatus -> print errorStatus
        Right _frame -> putStrLn "got some frame"
