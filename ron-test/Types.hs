{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import           RON.Schema.TH (mkReplicated)

[mkReplicated|
    (struct_lww TestRecursiveORSet
        (testRecSet (ORSet TestRecursiveORSet)))
|]
