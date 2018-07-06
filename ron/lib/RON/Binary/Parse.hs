{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RON.Binary.Parse where

import           Internal.Prelude

import           Attoparsec.Extra (Parser, anyWord8, endOfInputEx, label,
                                   parseOnlyL, string, takeL, withInputSize)
import qualified Data.Binary as Binary
import           Data.Bits (shiftR, (.&.))
import           Data.ZigZag (zzDecode64)

import           RON.Binary.Types (Desc (..), Size)
import           RON.Types (Atom (..), Frame, Op (..), UUID (..))

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

parse :: ByteStringL -> Either String Frame
parse = parseOnlyL $ parseFrame <* endOfInputEx

parseFrame :: Parser Frame
parseFrame = label "Frame" $ do
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
