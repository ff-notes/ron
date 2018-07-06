{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Binary.Serialize where

import           Internal.Prelude

import qualified Data.Binary as Binary
import           Data.Bits (shiftL, (.|.))
import           Data.ByteString.Lazy (cons)
import qualified Data.ByteString.Lazy as BSL
import           Data.ZigZag (zzEncode)

import           RON.Binary.Types (Desc (..), Size)
import           RON.Types (Atom (..), Frame, Op (..), UUID (..))

serialize :: Frame -> ByteStringL
serialize f = "RON2" <> bodySize <> body
  where
    body = foldMap serializeOp f
    bodySize = Binary.encode (fromIntegral $ BSL.length body :: Size)

serializeOp :: Op -> ByteStringL
serializeOp Op{..} =
    serializeWithDesc DOpRaw $ mconcat $
        [ serializeUuidType     opType
        , serializeUuidObject   opObject
        , serializeUuidEvent    opEvent
        , serializeUuidLocation opLocation
        ]
        ++ map serializeAtom opPayload
  where
    serializeUuidType     = serializeWithDesc DUuidType     . serializeUuid
    serializeUuidObject   = serializeWithDesc DUuidObject   . serializeUuid
    serializeUuidEvent    = serializeWithDesc DUuidEvent    . serializeUuid
    serializeUuidLocation = serializeWithDesc DUuidLocation . serializeUuid

serializeUuid :: UUID -> ByteStringL
serializeUuid (UUID x y) = Binary.encode x <> Binary.encode y

serializeAtomUuid :: UUID -> ByteStringL
serializeAtomUuid = serializeWithDesc DAtomUuid . serializeUuid

encodeDesc :: Desc -> Word4
encodeDesc = fromIntegral . fromEnum

serializeWithDesc
    :: Desc
    -> ByteStringL  -- ^ body
    -> ByteStringL
serializeWithDesc d body =
    descByte `cons`
    -- TODO lengthExtended <>
    body
  where
    len = BSL.length body
    descByte = encodeDesc d `shiftL` 4 .|. lengthField
    lengthField -- , lengthExtended)
        | d == DOpRaw = 0
        | len < 16    = fromIntegral len
        | len == 16   = 0
        | otherwise   = error "impossible"

serializeAtom :: Atom -> ByteStringL
serializeAtom = \case
    AUuid uuid -> serializeAtomUuid uuid
    AInteger int ->
        serializeWithDesc DAtomInteger $ Binary.encode $ zzEncode64 int
  where
    {-# INLINE zzEncode64 #-}
    zzEncode64 :: Int64 -> Word64
    zzEncode64 = zzEncode
