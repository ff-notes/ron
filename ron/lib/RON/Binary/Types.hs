{-# LANGUAGE LambdaCase #-}

-- | Common types for binary format (parser and serializer)
module RON.Binary.Types where

import RON.Prelude

type Size = Word32

-- | Data block descriptor
data Desc
    = -- op descriptors
      DOpClosed
    | DOpReduced
    | DOpHeader
    | DOpQueryHeader
    | -- op key descriptors
      DUuidReducer
    | DUuidObject
    | DUuidOp
    | DUuidRef
    | -- zipped uuid descriptors
      DAtomUuidZip
    | DUuidZipObject
    | DUuidZipOp
    | DUuidZipRef
    | -- atom descriptors
      DAtomUuid
    | DAtomInteger
    | DAtomString
    | DAtomFloat
    deriving (Enum, Eq, Show)

-- | Does the descriptor refer to an op
descIsOp :: Desc -> Bool
descIsOp = \case
    DOpClosed -> True
    DOpReduced -> True
    DOpHeader -> True
    DOpQueryHeader -> True
    _ -> False
