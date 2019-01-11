{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module RON.Util (
    ByteStringL,
    Instance (Instance),
    maxOn,
    minOn,
) where

import qualified Data.ByteString.Lazy as BSL

type ByteStringL = BSL.ByteString

data Instance c = forall a . c a => Instance a

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x < f y then y else x

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y = if f x < f y then x else y
