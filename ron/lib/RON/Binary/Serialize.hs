{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Binary.Serialize where

import           RON.Internal.Prelude

import qualified Data.Binary as Binary
import           Data.Bits (bit, shiftL, (.|.))
import           Data.ByteString.Lazy (cons)
import qualified Data.ByteString.Lazy as BSL
import           Data.ZigZag (zzEncode)

import           RON.Binary.Types (Desc (..), Size, descIsOp)
import           RON.Internal.Word (Word4, b0000, leastSignificant4, safeCast)
import           RON.Types (Atom (..), Chunk (..), Frame, Op (..),
                            ReducedChunk (..), UUID (..))

serialize :: Frame -> Either String ByteStringL
serialize frame = ("RON2" <>) <$> serializeBody
  where
    serializeBody = foldChunks $ map serializeChunk frame

    chunkSize :: Bool -> Int64 -> Either String ByteStringL
    chunkSize continue x
        | x < bit 31 = Right $ Binary.encode s'
        | otherwise  = Left $ "chunk size is too big: " ++ show x
      where
        s = fromIntegral x :: Size
        s'  | continue  = s .|. bit 31
            | otherwise = s

    foldChunks :: [ByteStringL] -> Either String ByteStringL
    foldChunks = \case
        []   -> chunkSize False 0
        [c]  -> (<> c) <$> chunkSize False (BSL.length c)
        c:cs ->
            mconcat <$>
            sequence [chunkSize True (BSL.length c), pure c, foldChunks cs]

serializeChunk :: Chunk -> ByteStringL
serializeChunk = \case
    Raw op         -> serializeOp DOpRaw op
    Reduced rchunk -> serializeReducedChunk rchunk

serializeOp :: Desc -> Op -> ByteStringL
serializeOp desc Op{..} =
    serializeWithDesc desc $ mconcat $
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
encodeDesc = leastSignificant4 . fromEnum

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
    descByte = safeCast (encodeDesc d) `shiftL` 4 .|. safeCast lengthField
    lengthField -- , lengthExtended)
        | descIsOp d  = b0000
        | len < 16    = leastSignificant4 len
        | len == 16   = b0000
        | otherwise   = error "impossible"

serializeAtom :: Atom -> ByteStringL
serializeAtom = \case
    AInteger i -> serializeWithDesc DAtomInteger $ Binary.encode $ zzEncode64 i
    AUuid    u -> serializeAtomUuid u
  where
    {-# INLINE zzEncode64 #-}
    zzEncode64 :: Int64 -> Word64
    zzEncode64 = zzEncode

serializeReducedChunk :: ReducedChunk -> ByteStringL
serializeReducedChunk ReducedChunk{chunkHeader, chunkIsQuery, chunkBody}
    =   serializeOp
            (if chunkIsQuery then DOpQueryHeader else DOpHeader) chunkHeader
    <>  foldMap (serializeOp DOpReduced) chunkBody
