{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import           RON.Data.RGA (RgaString)
import           RON.Storage.FS (Collection)
import qualified RON.Storage.FS as Storage

instance Collection RgaString where
    collectionName = "text"
