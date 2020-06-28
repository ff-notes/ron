{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module RON.UUID.Experimental (value) where

import           RON.Prelude

import           Data.Bits ((.&.), (.|.))

import           RON.Util.Word (Word60, ls60, safeCast)
import           RON.UUID (UUID (..))

-- data UuidFields = UuidFields
--     { uuidVariety :: !Word4
--     , uuidValue   :: !Word60
--     , uuidVariant :: !Word2
--     , uuidVersion :: !Word2
--     , uuidOrigin  :: !Word60
--     }

value :: Lens' UUID Word60
value =
  lens
    (\(UUID x _) -> ls60 x)
    (\(UUID x y) v -> UUID (x .&. 0x_F000_0000_0000_0000 .|. safeCast v) y)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}
