{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RON.Binary (parse, serialize) where

import           Internal.Prelude

import           Attoparsec.Extra (Parser, anyWord8, label, parseWhole, string,
                                   takeL, withInputSize)
import qualified Data.Binary as Binary
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString.Lazy (cons)
import qualified Data.ByteString.Lazy as BSL
import           Data.ZigZag (zzDecode64, zzEncode)

import           RON.Types (Atom (..), Frame, Op (..), UUID (..))

type Size = Word32

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

encodeDesc :: Desc -> Word4
encodeDesc = fromIntegral . fromEnum

parseDesc :: Parser (Desc, Size)
parseDesc = label "desc" $ do
    b <- label "start byte" anyWord8
    let typeCode = b `shiftR` 4
    let sizeCode = b .&. 0b1111
    let desc = toEnum $ fromIntegral typeCode
    size <- case (sizeCode, desc) of
        (0, DOpRaw) -> pure 0
        (0, _     ) -> pure 16
        _           -> pure $ fromIntegral sizeCode
    pure (desc, size)

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

parse :: ByteStringL -> Either String Frame
parse = parseWhole parseDescAndFrame

parseDescAndFrame :: Parser Frame
parseDescAndFrame = label "Frame" $ do
    _ <- string "RON2" <|> do
        magic <- takeL 4
        fail $ "unsupported magic sequence " ++ show magic
    size :: Size <- Binary.decode <$> takeL 4
    (consumed, ops) <- withInputSize $ parseOps $ fromIntegral size
    when (consumed /= fromIntegral size) $
        fail $
        "size mismatch: expected " ++ show size ++ ", got " ++ show consumed
    pure ops

parseOps :: Int -> Parser [Op]
parseOps = label "[Op]" . \case
    0        -> pure []
    expected -> do
        (consumed, op) <- withInputSize parseDescAndOp
        case compare consumed expected of
            LT -> (op :) <$> parseOps (expected - consumed)
            EQ -> pure [op]
            GT -> fail "impossible"

parseDescAndOp :: Parser Op
parseDescAndOp = label "d+Op" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $ fail $ "size = " ++ show size
    case desc of
        DOpRaw -> parseOp desc
        _      -> fail $ show desc

parseOp :: Desc -> Parser Op
parseOp = label "Op" . \case
    DOpRaw -> do
        opType     <- parseOpKey DUuidType
        opObject   <- parseOpKey DUuidObject
        opEvent    <- parseOpKey DUuidEvent
        opLocation <- parseOpKey DUuidLocation
        opPayload  <- parsePayload
        pure Op{..}
    desc -> fail $ show desc

parseOpKey :: Desc -> Parser UUID
parseOpKey expectedType = label "OpKey" $ do
    (desc, size) <- parseDesc
    let go = do
            guard $ desc == expectedType
            parseUuid size
    case desc of
        DUuidType     -> go
        DUuidObject   -> go
        DUuidEvent    -> go
        DUuidLocation -> go
        _             -> fail $ show desc

parseUuid :: Size -> Parser UUID
parseUuid size = label "UUID" $
    case size of
        16 -> do
            x <- Binary.decode <$> takeL 8
            y <- Binary.decode <$> takeL 8
            pure $ UUID x y
        _  -> fail "expected uuid of size 16"

parsePayload :: Parser [Atom]
parsePayload = label "payload" $ many parseAtom

parseAtom :: Parser Atom
parseAtom = label "Atom" $ do
    (desc, size) <- parseDesc
    case desc of
        DAtomUuid    -> AUuid    <$> parseUuid size
        DAtomInteger -> AInteger <$> parseInteger size
        _            -> fail "expected Atom"

-- big-endian, zigzag-coded, lengths 1..8
parseInteger :: Size -> Parser Int64
parseInteger size = label "Integer" $ do
    unless (size >= 1 && size <= 8) $ fail "integer size must be 1..8"
    unless (size == 8) $ fail "integer size /=8 not implemented"
    zzDecode64 . Binary.decode <$> takeL (fromIntegral size)

serializeAtom :: Atom -> ByteStringL
serializeAtom = \case
    AUuid uuid -> serializeAtomUuid uuid
    AInteger int ->
        serializeWithDesc DAtomInteger $ Binary.encode $ zzEncode64 int
  where
    {-# INLINE zzEncode64 #-}
    zzEncode64 :: Int64 -> Word64
    zzEncode64 = zzEncode
