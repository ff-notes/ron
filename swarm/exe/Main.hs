module Main (main) where

import qualified RON.UUID as UUID
import           Swarm.DB.Replica (get, newTextReplica)

main :: IO ()
main = do
    replica <- newTextReplica
    got <- get UUID.zero replica
    case got of
        Left errorStatus -> print errorStatus
        Right _frame -> putStrLn "got some frame"
