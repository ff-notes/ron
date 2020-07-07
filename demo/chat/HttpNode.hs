module HttpNode (worker) where

worker ::
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
worker _listen _peers = pure ()
