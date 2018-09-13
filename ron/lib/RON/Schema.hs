{-# LANGUAGE OverloadedStrings #-}

module RON.Schema where

import           Language.Haskell.TH (DecsQ)

import           RON.Internal.Prelude

data TAtom = TAInteger | TAString

data TBuiltin = TORSet Type | TRga Type | TVersionVector

data Type
    = TAtom TAtom
    | TBuiltin TBuiltin
    | TStructLww StructLww
    | TWrapper Wrapper

data StructLww = StructLww
    { slName    :: Text
    , slFields  :: Map Text (Annotated Type)
    }

data Wrapper = Wrapper Text Type

type Schema = [Type]

tChar :: Type
tChar = TWrapper $ Wrapper "Char" $ TAtom TAString

data Annotated t = Ann t [Annotation]

(//) :: t -> [Annotation] -> Annotated t
(//) = Ann

newtype Annotation = AnnHaskellType HaskellType

data HaskellType
    = THaskellList
    | THaskellHashSet

mkReplicated :: Schema -> DecsQ
mkReplicated _schema = pure []
