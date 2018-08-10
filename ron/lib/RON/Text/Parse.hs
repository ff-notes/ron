{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Parse
    ( chunks
    , frame
    , parseFrame
    , parseFrames
    , parseOp
    , parseUuid
    ) where

import           Prelude hiding (takeWhile)
import           RON.Internal.Prelude

import           Attoparsec.Extra (Parser, endOfInputEx, isSuccessful, label,
                                   match, option, parseOnlyL, satisfy, (??))
import           Data.Aeson as Json
import           Data.Attoparsec.ByteString.Char8 (anyChar, char, digit,
                                                   peekChar, peekChar',
                                                   skipSpace, takeWhile,
                                                   takeWhile1)
import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.Functor (($>))

import qualified RON.Base64 as Base64
import           RON.Internal.Word (Word2, b00, b01, b10, b11, safeCast)
import           RON.Types (Atom (..), Chunk (..), Frame, Op (..), OpTerm (..),
                            ReducedChunk (..), UUID (..))

parseFrame :: ByteStringL -> Either String Frame
parseFrame = parseOnlyL frame

chunks :: Parser [Chunk]
chunks = label "[Chunk]" $ go chunk
  where
    go pchunk =
        optional pchunk >>= \case
            Just (ch, lastOp) -> (ch :) <$> go (chunkCont lastOp)
            Nothing           -> pure []

-- | Returns a chunk and the last op in it
chunk :: Parser (Chunk, Op)
chunk = label "Chunk" $ chunkReduced <|> chunkRaw op

-- | Returns a chunk and the last op in it
chunkCont :: Op -> Parser (Chunk, Op)
chunkCont prev = label "Chunk-cont" $ chunkReduced <|> chunkRaw (opCont prev)

chunkReduced :: Parser (Chunk, Op)
chunkReduced = do
    (ch, x) <- rchunk
    pure (Reduced ch, x)

chunkRaw
    :: Parser Op  -- ^ start op parser, 'op' or 'opCont'
    -> Parser (Chunk, Op)
chunkRaw pop = do
    x <- pop
    t <- term
    unless (t == TRaw) $ fail "raw op must end with `;`"
    pure (Raw x, x)

-- | Returns a chunk and the last op in it
rchunk :: Parser (ReducedChunk, Op)
rchunk = label "ReducedChunk" $ do
    (chunkHeader, chunkIsQuery) <- header
    chunkBody <- ops
    let lastOp = case chunkBody of
            [] -> chunkHeader
            _  -> last chunkBody
    pure (ReducedChunk{..}, lastOp)

frame :: Parser Frame
frame = label "Frame" $ chunks <* (endOfFrame <|> endOfInputEx)

parseFrames :: ByteStringL -> Either String [Frame]
parseFrames = parseOnlyL $ many frameInStream <* endOfInputEx

frameInStream :: Parser Frame
frameInStream = label "Frame-stream" $ do
    chs <- chunks
    endOfFrame <|> (assertNotEmpty chs *> endOfInputEx)
    pure chs
  where
    assertNotEmpty chs = when (null chs) $ fail "empty frame must end with `.`"

parseOp :: ByteStringL -> Either String Op
parseOp = parseOnlyL $ op <* skipSpace <* endOfInputEx

parseUuid :: ByteStringL -> Either String UUID
parseUuid = parseOnlyL $ uuidUncompressed <* skipSpace <* endOfInputEx

endOfFrame :: Parser ()
endOfFrame = label "end of frame" $ void $ skipSpace *> char '.'

ops :: Parser [Op]
ops = label "Frame ops" $ go op <|> stop
  where
    go pop = do
        x <- pop
        t <- optional term
        unless (t == Just TReduced || isNothing t) $
            fail "reduced op may end with `,` only"
        xs <- go (opCont x) <|> stop
        pure $ x : xs
    stop = pure []

op :: Parser Op
op = label "Op" $ do
    opType     <- key "type"     '*' Nothing
    opObject   <- key "object"   '#' (Just opType)
    opEvent    <- key "event"    '@' (Just opObject)
    opLocation <- key "location" ':' (Just opEvent)
    opPayload  <- payload opObject
    pure Op{..}
  where
    key name keyChar prev = label name $
        skipSpace *> char keyChar *> uuid Nothing prev PrevOpSameKey

opCont :: Op -> Parser Op
opCont prev = label "Op-cont" $ do
    (hasTyp, opType)     <- key "type"     '*' (opType     prev) Nothing
    (hasObj, opObject)   <- key "object"   '#' (opObject   prev) (Just opType)
    (hasEvt, opEvent)    <- key "event"    '@' (opEvent    prev) (Just opObject)
    (hasLoc, opLocation) <- key "location" ':' (opLocation prev) (Just opEvent)
    unless (hasTyp || hasObj || hasEvt || hasLoc) $ fail "no key found"
    opPayload <- payload opObject
    pure Op{..}
  where
    key name keyChar prevOpSameKey sameOpPrevUuid = label name $ do
        skipSpace
        isKeyPresent <- isSuccessful $ char keyChar
        if isKeyPresent then do
            u <- uuid (Just prevOpSameKey) sameOpPrevUuid PrevOpSameKey
            pure (True, u)
        else
            -- no key => use previous key
            pure (False, prevOpSameKey)

data Base64Format = Ronic | Long
    deriving (Eq, Show)

uuidUncompressed :: Parser UUID
uuidUncompressed = label "UUID" $ uuidAsBase64DoubleWord <|> uuidRon

uuidRon :: Parser UUID
uuidRon = label "UUID-RON" $ do
    x <- do
        mvariety <- optional $ satisfy isUpperHexDigit <* "/"
        (format, word0) <- base64word
        word <- case (format, mvariety) of
            (Ronic, Nothing) -> pure $ '0' `BSC.cons` word0
            (Ronic, Just v ) -> pure $  v   `BS.cons` word0
            (Long,  Nothing) -> pure word0
            (Long,  Just _ ) -> "mixing RON variety with long UUID"
        Base64.decode64 word ?? "Base64 decoding error"
    y <- option 0 $ do
        mscheme <- Just <$> scheme <|> skipSpace $> Nothing
        (format, word) <- base64word
        mw <- case (format, mscheme) of
            (Ronic, Nothing   ) -> pure $ safeCast <$> Base64.decode60 word
            (Ronic, Just schem) -> pure $
                ((safeCast schem `shiftL` 60) .|.) . safeCast
                <$> Base64.decode60 word
            (Long, Nothing) -> pure $ Base64.decode64 word
            (Long, Just _ ) -> fail "mixing RON scheme with long UUID"
        mw ?? "Base64 decoding error"
    pure $ UUID x y

uuidAsBase64DoubleWord :: Parser UUID
uuidAsBase64DoubleWord = label "UUID-Base64-double-word" $ do
    xy <- takeWhile1 Base64.isLetter
    guard $ BS.length xy == 22
    maybe (fail "Base64 decoding error") pure $
        UUID
            <$> Base64.decode64 (BS.take 11 xy)
            <*> Base64.decode64 (BS.drop 11 xy)

base64word :: Parser (Base64Format, ByteString)
base64word = label "Base64 word" $ do
    word <- takeWhile Base64.isLetter
    let n = BS.length word
    if  | n == 0  ->
            peekChar >>= \case
                Just c  -> fail $ "unexpected " ++ show c
                Nothing -> fail "unexpected end of input"
        | n == 11 -> pure (Long, word)
        | n <  11 -> pure (Ronic, word)
        | True    -> fail "too long Base64 word"

isUpperHexDigit :: Word8 -> Bool
isUpperHexDigit c =
    (fromIntegral (c - fromIntegral (ord '0')) :: Word) <= 9 ||
    (fromIntegral (c - fromIntegral (ord 'A')) :: Word) <= 5

scheme :: Parser Word2
scheme = label "scheme" $
    anyChar >>= \case
        '$' -> pure b00
        '%' -> pure b01
        '+' -> pure b10
        '-' -> pure b11
        _   -> fail "not a scheme"

payload :: UUID -> Parser [Atom]
payload = label "payload" . go
  where
    go prevUuid = do
        ma <- optional $ atom prevUuid
        case ma of
            Nothing -> pure []
            Just a  -> (a :) <$> go newUuid
              where
                newUuid = case a of
                    AUuid u -> u
                    _       -> prevUuid

atom :: UUID -> Parser Atom
atom prevUuid = skipSpace *> atom'
  where
    atom' =
        char '=' *> skipSpace *> (AInteger <$> integer) <|>
        char '>' *> skipSpace *> (AUuid    <$> uuid'  ) <|>
        AString                            <$> string
    integer = read <$> (maybe id (:) <$> optional (char '-') <*> some digit)
    uuid'   = uuid Nothing (Just prevUuid) SameOpPrevUuid
    string  = do
        (bs, ()) <- match . void $ char '"' >> takeWhile (/= '"') >> char '"'
        case Json.decodeStrict bs of
            Just s  -> pure s
            Nothing -> fail "bad string"

data UuidCompressionBase = PrevOpSameKey | SameOpPrevUuid

uuid :: Maybe UUID -> Maybe UUID -> UuidCompressionBase -> Parser UUID
uuid prevOpSameKey sameOpPrevUuid = label "UUID" . go False
  where
    go allowEmpty position =
        peekChar' >>= \case
            '`' -> anyChar *> go True SameOpPrevUuid
            '(' -> reuse 4
            '[' -> reuse 5
            '{' -> reuse 6
            '}' -> reuse 7
            ']' -> reuse 8
            ')' -> reuse 9
            c   | Base64.isLetter c -> uuidUncompressed
                | allowEmpty -> case mprev of
                    Just u  -> pure u
                    Nothing -> fail "empty uuid"
                | otherwise -> fail $ "unsupported compression " ++ show c
      where

        mprev = case position of
            PrevOpSameKey   -> prevOpSameKey
            SameOpPrevUuid -> sameOpPrevUuid

        reuse :: Int -> Parser UUID
        reuse prefixLen = label "reuse" $ do
            void anyChar -- skip prefix compression mark
            UUID prevX prevY <-
                mprev ?? "can't reuse prefix whithout previous UUID"
            word <- takeWhile Base64.isLetter
            when (BS.length word > 10 - prefixLen) $
                fail "too long postfix for this prefix"
            newPart <- safeCast <$> Base64.decode60 word ?? "Base64.decode60"
            let prefix = prevX .&. complement 0 `shiftL` (60 - 6 * prefixLen)
                postfix = newPart `shiftR` (6 * prefixLen)
                x = prefix .|. postfix
            pure $ UUID x prevY

-- | Return 'Op' and 'chunkIsQuery'
header :: Parser (Op, Bool)
header = do
    x <- op
    t <- term
    case t of
        THeader -> pure (x, False)
        TQuery  -> pure (x, True)
        _       -> fail "not a header"

term :: Parser OpTerm
term = do
    skipSpace
    anyChar >>= \case
        '!' -> pure THeader
        '?' -> pure TQuery
        ',' -> pure TReduced
        ';' -> pure TRaw
        _   -> fail "not a term"
