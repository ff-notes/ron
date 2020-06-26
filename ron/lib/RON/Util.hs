{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module RON.Util (
    Instance (Instance),
) where

data Instance c = forall a . c a => Instance a
