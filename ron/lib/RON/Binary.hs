{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Binary (parse, serialize) where

import           Internal.Prelude

import           Attoparsec.Extra (Parser, anyWord8, label, parseWhole, string,
                                   takeL, withInputSize)
import           Data.Binary (Binary, decode, encode)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString.Lazy (cons)
import qualified Data.ByteString.Lazy as BSL

import           RON.Types (Frame, Op (..), UUID (..))

type Size = Word32

serialize :: Frame -> ByteStringL
serialize f =
    "RON" <> withDescriptor (DFrame Version_2_0) (foldMap serializeOp f)

serializeOp :: Op -> ByteStringL
serializeOp Op{..} =
    withDescriptor (DOp Raw) $ mconcat
        [ serializeUuid Type          opType
        , serializeUuid Object        opObject
        , serializeUuid Event         opEvent
        , serializeUuid RefOrLocation opLocation
        ]

serializeUuid :: UuidSubtype -> UUID -> ByteStringL
serializeUuid s (UUID x y) = withDescriptor (DUuid s) $ encode x <> encode y

type FrameVersion = Word8

pattern Version_2_0 :: FrameVersion
pattern Version_2_0 = 0b1000

data Descriptor
    = DFrame  FrameVersion
    | DOp     OpSubtype
    | DUuid   UuidSubtype
    | DUuidZ
    | DAtom
    deriving (Eq, Show)

data OpSubtype = Raw | Reduced | Header | QueryHeader
    deriving (Enum, Eq, Show)

data UuidSubtype = Type | Object | Event | RefOrLocation
    deriving (Enum, Eq, Show)

encodeDescriptorHeader :: Descriptor -> Word8
encodeDescriptorHeader = \case
    DFrame v -> v
    DOp    s -> encodeDescriptorHeader' 0b0000 s
    DUuid  s -> encodeDescriptorHeader' 0b0100 s
    DUuidZ   -> encodeDescriptorHeader' 0b1000 ()
    DAtom    -> encodeDescriptorHeader' 0b1100 ()
  where
    encodeDescriptorHeader' d s = d .|. fromIntegral (fromEnum s)

pattern LengthU8 :: Word8
pattern LengthU8  = 13

pattern LengthU16 :: Word8
pattern LengthU16 = 14

pattern LengthU32 :: Word8
pattern LengthU32 = 15

parseDescriptor :: Parser (Descriptor, Size)
parseDescriptor = label "descriptor" $ do
    b <- label "header" anyWord8
    let typeCode    = b `shiftR` 6
        subtypeCode = fromIntegral $ b `shiftR` 4 .&. 0b11
        sizeCode    = fromIntegral $ b .&. 0b1111
    let desc = case typeCode of
            0b00 -> DOp   $ toEnum subtypeCode
            0b01 -> DUuid $ toEnum subtypeCode
            0b10 -> DUuidZ
            0b11 -> DAtom
            _    -> error $ "impossible value for 2 bits: " ++ show typeCode
    size <- case sizeCode of
        LengthU32 -> decode  @Word32 <$> label "length 32" (takeL 4)
        LengthU16 -> decode' @Word16 <$> label "length 16" (takeL 2)
        LengthU8  -> decode' @Word8  <$> label "length 8"  (takeL 1)
        _         -> pure $ fromIntegral sizeCode
    pure (desc, size)
  where
    decode' :: forall int . (Binary int, Integral int) => ByteStringL -> Size
    decode' = fromIntegral . decode @int

parseFrameDescriptor :: Parser (FrameVersion, Size)
parseFrameDescriptor = label "frame descriptor" $ do
    b <- anyWord8
    let version  = b `shiftR` 4
        sizeCode = fromIntegral $ b .&. 0b1111
    size <- case sizeCode of
        LengthU32 ->                decode @Word32 <$> takeL 4
        LengthU16 -> fromIntegral . decode @Word16 <$> takeL 2
        LengthU8  -> fromIntegral . decode @Word8  <$> takeL 1
        _         -> pure $ fromIntegral sizeCode
    pure (version, size)

withDescriptor
    :: Descriptor
    -> ByteStringL  -- ^ body
    -> ByteStringL
withDescriptor d body = headerByte `cons` lengthExtended <> body
  where
    len = BSL.length body
    headerByte = encodeDescriptorHeader d `shiftL` 4 .|. lengthHeader
    (lengthHeader, lengthExtended)
        | len < 13      = (fromIntegral len, "")
        | len < 0x100   = (LengthU8,  encode (fromIntegral len :: Word8))
        | len < 0x10000 = (LengthU16, encode (fromIntegral len :: Word16))
        | otherwise     = (LengthU32, encode (fromIntegral len :: Word32))

parse :: ByteStringL -> Either String Frame
parse = parseWhole parseFrame

parseFrame :: Parser Frame
parseFrame = label "Frame" $ do
    _ <- string "RON" <|> do
        magic <- takeL 3
        fail $ "unsupported magic sequence " ++ show magic
    (version, size) <- parseFrameDescriptor
    case version of
        Version_2_0 -> pure ()
        _ -> do
            let major = version `shiftR` 2
                minor = version .&. 0b11
            fail $ "unsupported version " ++ show major ++ "." ++ show minor
    (consumed, ops) <- withInputSize $ many parseOp
    when (consumed /= fromIntegral size) $ fail "size mismatch"
    pure ops

parseOp :: Parser Op
parseOp = label "Op" $ do
    (desc, size) <- parseDescriptor
    case desc of
        DOp subtype -> do
            (consumed, op) <- withInputSize $ parseOpContent subtype
            when (consumed /= fromIntegral size) $ fail "op size mismatch"
            pure op
        _ -> fail $ "expected Op, got " ++ show desc

parseOpContent :: OpSubtype -> Parser Op
parseOpContent = \case
    Raw -> do
        opType     <- parseUuid Type
        opObject   <- parseUuid Object
        opEvent    <- parseUuid Event
        opLocation <- parseUuid RefOrLocation
        pure Op{..}
    Reduced     -> fail "decoding Reduced Op not implemented"
    Header      -> fail "decoding Header Op not implemented"
    QueryHeader -> fail "decoding Query Header Op not implemented"

parseUuid :: UuidSubtype -> Parser UUID
parseUuid expectedSubtype = label "UUID" $ do
    (desc, size) <- parseDescriptor
    case desc of
        DUuid subtype
            | subtype == expectedSubtype -> case size of
                16 -> do
                    x <- decode <$> takeL 8
                    y <- decode <$> takeL 8
                    pure $ UUID x y
                _  -> fail "expected uuid of size 16"
            | otherwise -> fail $ "expected " ++ show expectedSubtype
        _ -> fail "expected DUuid"
