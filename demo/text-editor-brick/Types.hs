{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Types where

import RON.Data.RGA (RgaString)
import RON.Storage.FS (Collection)
import RON.Storage.FS qualified as Storage

instance Collection RgaString where
    collectionName = "text"
