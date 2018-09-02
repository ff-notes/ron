{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module RON.Binary.Parse where

import           RON.Internal.Prelude

import           Attoparsec.Extra (Parser, anyWord8, endOfInputEx, label,
                                   parseOnlyL, takeL, withInputSize)
import qualified Attoparsec.Extra as Atto
import qualified Data.Binary as Binary
import           Data.Binary.Get (getDoublebe, runGet)
import           Data.Bits (shiftR, testBit, (.&.))
import           Data.ByteString.Lazy (cons, toStrict)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ZigZag (zzDecode64)

import           RON.Binary.Types (Desc (..), Size, descIsOp)
import           RON.Internal.Word (safeCast)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            Chunk (Query, Raw, Value), Frame, Op (..),
                            Op' (..), OpTerm (THeader, TQuery, TRaw, TReduced),
                            RChunk (..), UUID (UUID))

parseDesc :: Parser (Desc, Size)
parseDesc = label "desc" $ do
    b <- label "start byte" anyWord8
    let typeCode = b `shiftR` 4
    let sizeCode = b .&. 0b1111
    let desc = toEnum $ fromIntegral typeCode
    size <- case (sizeCode, desc) of
        (0, DAtomString)    -> extendedLength
        (0, d) | descIsOp d -> pure 0
        (0, _)              -> pure 16
        _                   -> pure $ fromIntegral sizeCode
    pure (desc, size)

extendedLength :: Parser Size
extendedLength = do
    b <- anyWord8
    if testBit b 7 then do
        bbb <- takeL 3
        pure $ leastSignificant31 $ Binary.decode (b `cons` bbb)
    else
        pure $ safeCast b

parse :: ByteStringL -> Either String Frame
parse = parseOnlyL $ parseFrame <* endOfInputEx

parseFrame :: Parser Frame
parseFrame = label "Frame" $ do
    _ <- Atto.string "RON2" <|> do
        magic <- takeL 4
        fail $ "unsupported magic sequence " ++ show magic
    parseChunks

parseChunks :: Parser [Chunk]
parseChunks = do
    size :: Size <- Binary.decode <$> takeL 4
    if  | testBit size 31 ->
            liftA2 (:) (parseChunk $ leastSignificant31 size) parseChunks
        | size > 0 ->
            (:[]) <$> parseChunk size
        | True ->
            pure []

leastSignificant31 :: Word32 -> Word32
leastSignificant31 x = x .&. 0x7FFFFFFF

parseChunk :: Size -> Parser Chunk
parseChunk size = label "Chunk" $ do
    (consumed0, (term, op)) <- withInputSize parseDescAndOp
    let parseReducedChunk rchunkHeader isQuery = do
            rchunkBody <- parseReducedOps $ fromIntegral size - consumed0
            pure $ (if isQuery then Query else Value) RChunk{..}
    case term of
        THeader  -> parseReducedChunk op False
        TQuery   -> parseReducedChunk op True
        TReduced -> fail "reduced op without a chunk"
        TRaw     -> assertSize size consumed0 $> Raw op

assertSize :: Monad f => Size -> Int -> f ()
assertSize expected consumed =
    when (consumed /= fromIntegral expected) $
    fail $
    "size mismatch: expected " ++ show expected ++ ", got " ++ show consumed

parseReducedOps :: Int -> Parser [Op]
parseReducedOps = label "[Op]" . go
  where
    go = \case
        0        -> pure []
        expected -> do
            (consumed, (TReduced, op)) <- withInputSize parseDescAndOp
            case compare consumed expected of
                LT -> (op :) <$> go (expected - consumed)
                EQ -> pure [op]
                GT -> fail "impossible"

parseDescAndOp :: Parser (OpTerm, Op)
parseDescAndOp = label "d+Op" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $
        fail $ "desc = " ++ show desc ++ ", size = " ++ show size
    case desc of
        DOpRaw          -> (TRaw,)      <$> parseOp
        DOpReduced      -> (TReduced,)  <$> parseOp
        DOpHeader       -> (THeader,)   <$> parseOp
        DOpQueryHeader  -> (TQuery,)    <$> parseOp
        _               -> fail $ "unimplemented " ++ show desc

parseOp :: Parser Op
parseOp = label "Op" $ do
    opType    <- parseOpKey DUuidType
    opObject  <- parseOpKey DUuidObject
    opEvent   <- parseOpKey DUuidEvent
    opRef     <- parseOpKey DUuidRef
    opPayload <- parsePayload
    pure Op{op' = Op'{..}, ..}

parseOpKey :: Desc -> Parser UUID
parseOpKey expectedType = label "OpKey" $ do
    (desc, size) <- parseDesc
    let go = do
            guard $ desc == expectedType
            uuid size
    case desc of
        DUuidType   -> go
        DUuidObject -> go
        DUuidEvent  -> go
        DUuidRef    -> go
        _           -> fail $ show desc

uuid :: Size -> Parser UUID
uuid size = label "UUID" $
    case size of
        16 -> do
            x <- Binary.decode <$> takeL 8
            y <- Binary.decode <$> takeL 8
            pure $ UUID x y
        _  -> fail "expected uuid of size 16"

parsePayload :: Parser [Atom]
parsePayload = label "payload" $ many atom

atom :: Parser Atom
atom = label "Atom" $ do
    (desc, size) <- parseDesc
    case desc of
        DAtomFloat   -> AFloat   <$> float   size
        DAtomInteger -> AInteger <$> integer size
        DAtomString  -> AString  <$> string  size
        DAtomUuid    -> AUuid    <$> uuid    size
        _            -> fail "expected Atom"

parseAtom :: ByteStringL -> Either String Atom
parseAtom = parseOnlyL $ atom <* endOfInputEx

float :: Size -> Parser Double
float = \case
    8 -> runGet getDoublebe <$> takeL 8
    _ -> undefined

-- big-endian, zigzag-coded, lengths 1..8
integer :: Size -> Parser Int64
integer size = label "Integer" $ do
    unless (size >= 1 && size <= 8) $ fail "integer size must be 1..8"
    unless (size == 8) $ fail "integer size /=8 not implemented"
    zzDecode64 . Binary.decode <$> takeL (fromIntegral size)

string :: Size -> Parser Text
string size = decodeUtf8 . toStrict <$> takeL (fromIntegral size)

parseString :: ByteStringL -> Either String Text
parseString bs =
    parseOnlyL (string (fromIntegral $ BSL.length bs) <* endOfInputEx) bs
