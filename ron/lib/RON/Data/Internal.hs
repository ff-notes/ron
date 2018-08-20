module RON.Data.Internal where

import           RON.Types (Chunk, UUID)

-- | Reduce all chunks of specific type and object in the frame
type Reducer = UUID -> [Chunk] -> [Chunk]
