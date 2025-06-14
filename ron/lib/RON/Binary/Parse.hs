{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Binary parser elements
module RON.Binary.Parse (
    parse,
    parseAtom,
    parseString,
) where

import           RON.Prelude

import           Attoparsec.Extra (Parser, anyWord8, endOfInputEx, label,
                                   parseOnly, takeL, withInputSize)
import qualified Attoparsec.Extra as Atto
import qualified Data.Binary as Binary
import           Data.Binary.Get (getDoublebe, runGet)
import           Data.Bits (shiftR, testBit, (.&.))
import           Data.ByteString.Lazy (cons, toStrict)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text.Encoding (decodeUtf8)
import           Data.ZigZag (zzDecode64)

import           RON.Binary.Types (Desc (..), Size, descIsOp)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            ClosedOp (..), Op (..),
                            OpTerm (TClosed, THeader, TQuery, TReduced),
                            Payload, UUID (UUID),
                            WireChunk (Closed, Query, Value), WireFrame,
                            WireReducedChunk (..))
import           RON.Util.Word (safeCast)

-- | 'Parser' for descriptor
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

-- | 'Parser' for extended length field
extendedLength :: Parser Size
extendedLength = do
    b <- anyWord8
    if testBit b 7 then do
        bbb <- takeL 3
        pure $ leastSignificant31 $ Binary.decode (b `cons` bbb)
    else
        pure $ safeCast b

-- | Parse frame
parse :: ByteStringL -> Either String WireFrame
parse = parseOnly $ parseFrame <* endOfInputEx

-- | 'Parser' for frame
parseFrame :: Parser WireFrame
parseFrame = label "WireFrame" $ do
    _ <- Atto.string "RON2" <|> do
        magic <- takeL 4
        fail $ "unsupported magic sequence " ++ show magic
    parseChunks

-- | 'Parser' for chunk sequence
parseChunks :: Parser [WireChunk]
parseChunks = do
    size :: Size <- Binary.decode <$> takeL 4
    if  | testBit size 31 ->
            liftA2 (:) (parseChunk $ leastSignificant31 size) parseChunks
        | size > 0 ->
            (:[]) <$> parseChunk size
        | True ->
            pure []

-- | Clear upper bit of 'Word32'
leastSignificant31 :: Word32 -> Word32
leastSignificant31 x = x .&. 0x7FFFFFFF

-- | 'Parser' for a chunk
parseChunk
    :: Size  -- ^ expected input length
    -> Parser WireChunk
parseChunk size = label "WireChunk" $ do
    (consumed0, (term, op)) <- withInputSize parseDescAndClosedOp
    let parseReducedChunk wrcHeader isQuery = do
            wrcBody <- parseReducedOps $ fromIntegral size - consumed0
            pure $ (if isQuery then Query else Value) WireReducedChunk{..}
    case term of
        THeader  -> parseReducedChunk op False
        TQuery   -> parseReducedChunk op True
        TReduced -> fail "reduced op without a chunk"
        TClosed  -> assertSize size consumed0 $> Closed op

-- | Assert that is such as expected
assertSize :: MonadFail f => Size -> Int -> f ()
assertSize expected consumed =
    when (consumed /= fromIntegral expected) $
    fail $
    "size mismatch: expected " ++ show expected ++ ", got " ++ show consumed

-- | 'Parser' for a sequence of reduced ops
parseReducedOps :: Int -> Parser [Op]
parseReducedOps = label "[Op]" . go
  where
    go = \case
        0        -> pure []
        expected -> do
            (consumed, (TReduced, op)) <- withInputSize parseDescAndReducedOp
            case compare consumed expected of
                LT -> (op :) <$> go (expected - consumed)
                EQ -> pure [op]
                GT -> fail "impossible"

-- | 'Parser' for closed op, returning the op's terminator along with the op
parseDescAndClosedOp :: Parser (OpTerm, ClosedOp)
parseDescAndClosedOp = label "d+ClosedOp" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $
        fail $ "desc = " ++ show desc ++ ", size = " ++ show size
    case desc of
        DOpClosed       -> (TClosed,)   <$> parseClosedOp
        DOpHeader       -> (THeader,)   <$> parseClosedOp
        DOpQueryHeader  -> (TQuery,)    <$> parseClosedOp
        _               -> fail $ "unimplemented " ++ show desc

-- | 'Parser' for reduced op, returning the op's terminator along with the op
parseDescAndReducedOp :: Parser (OpTerm, Op)
parseDescAndReducedOp = label "d+ClosedOp" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $
        fail $ "desc = " ++ show desc ++ ", size = " ++ show size
    case desc of
        DOpReduced      -> (TReduced,)  <$> parseOpenOp
        _               -> fail $ "unimplemented " ++ show desc

-- | 'Parser' for closed op without terminator
parseClosedOp :: Parser ClosedOp
parseClosedOp = label "ClosedOp" $ do
    reducerId <- parseOpKey DUuidReducer
    objectId  <- parseOpKey DUuidObject
    op        <- parseOpenOp
    pure ClosedOp{..}

-- | 'Parser' for reduced op without terminator
parseOpenOp :: Parser Op
parseOpenOp = label "Op" $ do
    opId    <- parseOpKey DUuidOp
    refId   <- parseOpKey DUuidRef
    payload <- parsePayload
    pure Op{..}

-- | 'Parser' for an op key (type, object, event, or reference)
parseOpKey :: Desc -> Parser UUID
parseOpKey expectedType = label "OpKey" $ do
    (desc, size) <- parseDesc
    let go = do
            guard $ desc == expectedType
            uuid size
    case desc of
        DUuidReducer -> go
        DUuidObject  -> go
        DUuidOp      -> go
        DUuidRef     -> go
        _            -> fail $ show desc

-- | 'Parser' for UUID
uuid
    :: Size  -- ^ expected input length
    -> Parser UUID
uuid size = label "UUID" $
    case size of
        16 -> do
            x <- Binary.decode <$> takeL 8
            y <- Binary.decode <$> takeL 8
            pure $ UUID x y
        _  -> fail "expected uuid of size 16"

-- | 'Parser' for a payload (sequence of atoms)
parsePayload :: Parser Payload
parsePayload = label "payload" $ many atom

-- | 'Parser' for an atom
atom :: Parser Atom
atom = label "Atom" $ do
    (desc, size) <- parseDesc
    case desc of
        DAtomFloat   -> AFloat   <$> float   size
        DAtomInteger -> AInteger <$> integer size
        DAtomString  -> AString  <$> string  size
        DAtomUuid    -> AUuid    <$> uuid    size
        _            -> fail "expected Atom"

-- | Parse an 'Atom'
parseAtom :: ByteStringL -> Either String Atom
parseAtom = parseOnly $ atom <* endOfInputEx

-- | 'Parser' for a float atom
float
    :: Size  -- ^ expected input length
    -> Parser Double
float = \case
    8 -> runGet getDoublebe <$> takeL 8
    _ -> undefined

-- | 'Parser' for an integer atom
integer
    :: Size  -- ^ expected input length
    -> Parser Int64
integer size = label "Integer" $ do
    -- big-endian, zigzag-coded, lengths 1..8
    unless (size >= 1 && size <= 8) $ fail "integer size must be 1..8"
    unless (size == 8) $ fail "integer size /=8 not implemented"
    zzDecode64 . Binary.decode <$> takeL (fromIntegral size)

-- | 'Parser' for an string
string
    :: Size  -- ^ expected input length
    -> Parser Text
string size = decodeUtf8 . toStrict <$> takeL (fromIntegral size)

-- | Parse a string atom
parseString :: ByteStringL -> Either String Text
parseString bs =
    parseOnly (string (fromIntegral $ BSL.length bs) <* endOfInputEx) bs
