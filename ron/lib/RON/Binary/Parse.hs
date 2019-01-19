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

import           Prelude hiding (fail)
import           RON.Internal.Prelude

import           Attoparsec.Extra (Parser, anyWord8, endOfInputEx, label,
                                   parseOnlyL, takeL, withInputSize)
import qualified Attoparsec.Extra as Atto
import           Control.Monad.Fail (MonadFail, fail)
import qualified Data.Binary as Binary
import           Data.Binary.Get (getDoublebe, runGet)
import           Data.Bits (shiftR, testBit, (.&.))
import           Data.ByteString.Lazy (cons, toStrict)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text.Encoding (decodeUtf8)
import           Data.ZigZag (zzDecode64)

import           RON.Binary.Types (Desc (..), Size, descIsOp)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid), Op (..),
                            OpTerm (THeader, TQuery, TRaw, TReduced),
                            RawOp (..), UUID (UUID),
                            WireChunk (Query, Raw, Value), WireFrame,
                            WireReducedChunk (..))
import           RON.Util (ByteStringL)
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
parse = parseOnlyL $ parseFrame <* endOfInputEx

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
    (consumed0, (term, op)) <- withInputSize parseDescAndRawOp
    let parseReducedChunk wrcHeader isQuery = do
            wrcBody <- parseReducedOps $ fromIntegral size - consumed0
            pure $ (if isQuery then Query else Value) WireReducedChunk{..}
    case term of
        THeader  -> parseReducedChunk op False
        TQuery   -> parseReducedChunk op True
        TReduced -> fail "reduced op without a chunk"
        TRaw     -> assertSize size consumed0 $> Raw op

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

-- | 'Parser' for raw op, returning the op's terminator along with the op
parseDescAndRawOp :: Parser (OpTerm, RawOp)
parseDescAndRawOp = label "d+RawOp" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $
        fail $ "desc = " ++ show desc ++ ", size = " ++ show size
    case desc of
        DOpRaw          -> (TRaw,)      <$> parseRawOp
        DOpHeader       -> (THeader,)   <$> parseRawOp
        DOpQueryHeader  -> (TQuery,)    <$> parseRawOp
        _               -> fail $ "unimplemented " ++ show desc

-- | 'Parser' for reduced op, returning the op's terminator along with the op
parseDescAndReducedOp :: Parser (OpTerm, Op)
parseDescAndReducedOp = label "d+RawOp" $ do
    (desc, size) <- parseDesc
    unless (size == 0) $
        fail $ "desc = " ++ show desc ++ ", size = " ++ show size
    case desc of
        DOpReduced      -> (TReduced,)  <$> parseReducedOp
        _               -> fail $ "unimplemented " ++ show desc

-- | 'Parser' for raw op without terminator
parseRawOp :: Parser RawOp
parseRawOp = label "RawOp" $ do
    opType   <- parseOpKey DUuidType
    opObject <- parseOpKey DUuidObject
    op       <- parseReducedOp
    pure RawOp{..}

-- | 'Parser' for reduced op without terminator
parseReducedOp :: Parser Op
parseReducedOp = label "Op" $ do
    opEvent   <- parseOpKey DUuidEvent
    opRef     <- parseOpKey DUuidRef
    opPayload <- parsePayload
    pure Op{..}

-- | 'Parser' for an op key (type, object, event, or reference)
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
parsePayload :: Parser [Atom]
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
parseAtom = parseOnlyL $ atom <* endOfInputEx

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
    parseOnlyL (string (fromIntegral $ BSL.length bs) <* endOfInputEx) bs
