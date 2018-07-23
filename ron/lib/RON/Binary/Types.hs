{-# LANGUAGE LambdaCase #-}

module RON.Binary.Types where

import           Internal.Prelude

type Size = Word32

data Desc

    = DOpRaw
    | DOpReduced
    | DOpHeader
    | DOpQueryHeader

    | DUuidType
    | DUuidObject
    | DUuidEvent
    | DUuidLocation

    | DAtomUuidZip
    | DUuidZipObject
    | DUuidZipEvent
    | DUuidZipLocation

    | DAtomUuid
    | DAtomInteger
    | DAtomString
    | DAtomFloat

    deriving (Enum, Eq, Show)

descIsOp :: Desc -> Bool
descIsOp = \case
    DOpRaw          -> True
    DOpReduced      -> True
    DOpHeader       -> True
    DOpQueryHeader  -> True
    _               -> False
