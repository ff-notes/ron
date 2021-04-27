{-# LANGUAGE RankNTypes #-}

module RON.UUID.Experimental (variety, value, variant, version, origin) where

import           RON.Prelude

import           Data.Bits (shiftL, shiftR, (.&.), (.|.))

import           RON.UUID (UUID (..))
import           RON.Util.Word (Word2, Word4, Word60, leastSignificant2,
                                leastSignificant4, leastSignificant60, safeCast)

-- data UuidFields = UuidFields
--     { uuidVariety :: !Word4
--     , uuidValue   :: !Word60
--     , uuidVariant :: !Word2
--     , uuidVersion :: !Word2
--     , uuidOrigin  :: !Word60
--     }

variety :: Lens' UUID Word4
variety =
  lens
    (\(UUID x _) -> leastSignificant4 $ x `shiftR` 60)
    (\(UUID x y) v ->
      UUID (x .&. 0x0FFFFFFFFFFFFFFF .|. (safeCast v `shiftL` 60)) y)

value :: Lens' UUID Word60
value =
  lens
    (\(UUID x _) -> leastSignificant60 x)
    (\(UUID x y) v -> UUID (x .&. 0xF000000000000000 .|. safeCast v) y)

variant :: Lens' UUID Word2
variant =
  lens
    (\(UUID _ y) -> leastSignificant2 $ y `shiftR` 62)
    (\(UUID x y) v ->
      UUID x (y .&. 0x3FFFFFFFFFFFFFFF .|. (safeCast v `shiftL` 62)))

version :: Lens' UUID Word2
version =
  lens
    (\(UUID _ y) -> leastSignificant2 $ y `shiftR` 60)
    (\(UUID x y) v ->
      UUID x (y .&. 0xCFFFFFFFFFFFFFFF .|. (safeCast v `shiftL` 60)))

origin :: Lens' UUID Word60
origin =
  lens
    (\(UUID _ y) -> leastSignificant60 y)
    (\(UUID x y) v -> UUID x (y .&. 0xF000000000000000 .|. safeCast v))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}
