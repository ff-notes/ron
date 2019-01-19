{-# LANGUAGE LambdaCase #-}

-- | Common types for binary format (parser and serializer)
module RON.Binary.Types where

type Size = Word32

-- | Data block descriptor
data Desc

    = DOpRaw
    | DOpReduced
    | DOpHeader
    | DOpQueryHeader

    | DUuidType
    | DUuidObject
    | DUuidEvent
    | DUuidRef

    | DAtomUuidZip
    | DUuidZipObject
    | DUuidZipEvent
    | DUuidZipRef

    | DAtomUuid
    | DAtomInteger
    | DAtomString
    | DAtomFloat

    deriving (Enum, Eq, Show)

-- | Does the descriptor refer to an op
descIsOp :: Desc -> Bool
descIsOp = \case
    DOpRaw          -> True
    DOpReduced      -> True
    DOpHeader       -> True
    DOpQueryHeader  -> True
    _               -> False
