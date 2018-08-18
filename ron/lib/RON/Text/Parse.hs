{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Parse
    ( parseAtom
    , parseFrame
    , parseFrames
    , parseOp
    , parseString
    , parseUuid
    ) where

import           Prelude hiding (takeWhile)
import           RON.Internal.Prelude

import           Attoparsec.Extra (Parser, char, endOfInputEx, isSuccessful,
                                   label, manyTill, parseOnlyL, satisfy, (<+>),
                                   (??))
import qualified Data.Aeson as Json
import           Data.Attoparsec.ByteString.Char8 (anyChar, decimal, double,
                                                   signed, skipSpace, takeWhile,
                                                   takeWhile1)
import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.Text (Text)

import qualified RON.Base64 as Base64
import           RON.Internal.Word (Word2, Word4, Word60, b00, b0000, b01, b10,
                                    b11, ls60, safeCast)
import           RON.Types (Atom (..), Chunk (..), Frame, Op (..), OpTerm (..),
                            ReducedChunk (..), UUID (..))
import           RON.UUID (UuidFields (..))
import qualified RON.UUID as UUID

parseFrame :: ByteStringL -> Either String Frame
parseFrame = parseOnlyL frame

chunksTill :: Parser () -> Parser [Chunk]
chunksTill end = label "[Chunk]" $ go pChunk
  where
    go pchunk = do
        skipSpace
        atEnd <- isSuccessful end
        if atEnd then
            pure []
        else do
            (ch, lastOp) <- pchunk
            (ch :) <$> go (chunkCont lastOp)

-- | Returns a chunk and the last op in it
pChunk :: Parser (Chunk, Op)
pChunk = label "Chunk" $ rchunk <+> chunkRaw op

-- | Returns a chunk and the last op in it
chunkCont :: Op -> Parser (Chunk, Op)
chunkCont prev = label "Chunk-cont" $ rchunk <+> chunkRaw (opCont prev)

chunkRaw
    :: Parser Op  -- ^ start op parser, 'op' or 'opCont'
    -> Parser (Chunk, Op)
chunkRaw pop = label "Chunk-raw" $ do
    skipSpace
    x <- pop
    skipSpace
    void $ char ';'
    pure (Raw x, x)

-- | Returns a chunk and the last op in it
rchunk :: Parser (Chunk, Op)
rchunk = label "Chunk-reduced" $ do
    (chunkHeader, isQuery) <- header
    chunkBody <- reducedOps chunkHeader <|> stop
    let lastOp = case chunkBody of
            [] -> chunkHeader
            _  -> last chunkBody
    pure ((if isQuery then Query else State) ReducedChunk{..}, lastOp)
  where
    reducedOps y = do
        skipSpace
        x <- opCont y
        t <- optional term
        unless (t == Just TReduced || isNothing t) $
            fail "reduced op may end with `,` only"
        xs <- reducedOps x <|> stop
        pure $ x : xs
    stop = pure []

frame :: Parser Frame
frame = label "Frame" $ chunksTill (endOfFrame <|> endOfInputEx)

parseFrames :: ByteStringL -> Either String [Frame]
parseFrames = parseOnlyL $ manyTill frameInStream endOfInputEx

frameInStream :: Parser Frame
frameInStream = label "Frame-stream" $ chunksTill endOfFrame

parseOp :: ByteStringL -> Either String Op
parseOp = parseOnlyL $ op <* skipSpace <* endOfInputEx

parseUuid :: ByteStringL -> Either String UUID
parseUuid = parseOnlyL $ uuidUncompressed <* skipSpace <* endOfInputEx

endOfFrame :: Parser ()
endOfFrame = label "end of frame" $ void $ skipSpace *> char '.'

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

uuid :: Maybe UUID -> Maybe UUID -> UuidZipBase -> Parser UUID
uuid prevOpSameKey sameOpPrevUuid defaultZipBase = label "UUID" $
    uuid22 <+> uuid11 <+> uuidZip prevOpSameKey sameOpPrevUuid defaultZipBase

uuidUncompressed :: Parser UUID
uuidUncompressed =
    label "UUID-uncompressed" $ uuid Nothing Nothing PrevOpSameKey

uuid11 :: Parser UUID
uuid11 = label "UUID-RON-11-letter-value" $ do
    rawX <- base64word 11
    guard $ BS.length rawX == 11
    x <- Base64.decode64 rawX ?? fail "Base64.decode64"
    skipSpace
    rawScheme <- optional pScheme
    rawOrigin <- optional $ base64word $ maybe 11 (const 10) rawScheme
    y <- case (rawScheme, BS.length <$> rawOrigin) of
        (Nothing, Just 11) ->
            case rawOrigin of
                Nothing     -> pure 0
                Just origin -> Base64.decode64 origin ?? fail "Base64.decode64"
        _ -> do
            origin <- case rawOrigin of
                Nothing     -> pure $ ls60 0
                Just origin -> Base64.decode60 origin ?? fail "Base64.decode60"
            pure $ UUID.buildY b00 (fromMaybe b00 rawScheme) origin
    pure $ UUID x y

data UuidZipBase = PrevOpSameKey | SameOpPrevUuid

uuidZip :: Maybe UUID -> Maybe UUID -> UuidZipBase -> Parser UUID
uuidZip prevOpSameKey sameOpPrevUuid defaultZipBase = label "UUID-zip" $ do
    changeZipBase <- isSuccessful $ char '`'
    rawVariety <- optional pVariety
    rawReuseValue <- optional pReuse
    rawValue <- optional $ base64word60 $ 10 - fromMaybe 0 rawReuseValue
    skipSpace
    rawScheme <- optional pScheme
    rawReuseOrigin <- optional pReuse
    rawOrigin <- optional $ base64word60 $ 10 - fromMaybe 0 rawReuseOrigin

    let isReusing =
            changeZipBase || isJust rawReuseValue || isJust rawReuseOrigin
    unless (isReusing || isJust rawVariety || isJust rawValue
            || isJust rawScheme || isJust rawOrigin) $
        fail "Empty UUID"

    if isReusing then do
        let mprev = whichPrev changeZipBase
        prev <- UUID.split <$> mprev ?? fail "No previous UUID to reuse"
        if uuidVariant prev /= b00 then
            undefined
        else do
            uuidVariety <- pure $ fromMaybe (uuidVariety prev) rawVariety
            uuidValue <- pure $ reuse rawReuseValue rawValue (uuidValue prev)
            let uuidVariant = b00
            uuidScheme <- pure $ fromMaybe (uuidScheme prev) rawScheme
            uuidOrigin <-
                pure $ reuse rawReuseOrigin rawOrigin (uuidOrigin prev)
            pure $ UUID.build UuidFields{..}
    else
        pure $ UUID.build UuidFields
            { uuidVariety = fromMaybe b0000    rawVariety
            , uuidValue   = fromMaybe (ls60 0) rawValue
            , uuidVariant = b00
            , uuidScheme  = fromMaybe b00      rawScheme
            , uuidOrigin  = fromMaybe (ls60 0) rawOrigin
            }
  where

    whichPrev changeZipBase
        | changeZipBase = sameOpPrevUuid
        | otherwise = case defaultZipBase of
            PrevOpSameKey  -> prevOpSameKey
            SameOpPrevUuid -> sameOpPrevUuid

    reuse :: Maybe Int -> Maybe Word60 -> Word60 -> Word60
    reuse Nothing          Nothing    prev = prev
    reuse Nothing          (Just new) _    = new
    reuse (Just prefixLen) Nothing    prev =
        ls60 $ safeCast prev .&. complement 0 `shiftL` (60 - 6 * prefixLen)
    reuse (Just prefixLen) (Just new) prev = ls60 $ prefix .|. postfix
      where
        prefix  = safeCast prev .&. complement 0 `shiftL` (60 - 6 * prefixLen)
        postfix = safeCast new `shiftR` (6 * prefixLen)

pReuse :: Parser Int
pReuse = anyChar >>= \case
    '(' -> pure 4
    '[' -> pure 5
    '{' -> pure 6
    '}' -> pure 7
    ']' -> pure 8
    ')' -> pure 9
    _   -> fail "not a reuse symbol"

uuid22 :: Parser UUID
uuid22 = label "UUID-Base64-double-word" $ do
    xy <- base64word 22
    guard $ BS.length xy == 22
    maybe (fail "Base64 decoding error") pure $
        UUID
            <$> Base64.decode64 (BS.take 11 xy)
            <*> Base64.decode64 (BS.drop 11 xy)

base64word :: Int -> Parser ByteString
base64word maxSize = label "Base64 word" $ do
    word <- takeWhile1 Base64.isLetter
    guard $ BS.length word <= maxSize
    pure word

base64word60 :: Int -> Parser Word60
base64word60 maxSize = label "Base64 word60" $ do
    word <- base64word maxSize
    Base64.decode60 word ?? fail "decode60"

isUpperHexDigit :: Word8 -> Bool
isUpperHexDigit c =
    (fromIntegral (c - fromIntegral (ord '0')) :: Word) <= 9 ||
    (fromIntegral (c - fromIntegral (ord 'A')) :: Word) <= 5

pVariety :: Parser Word4
pVariety = label "variety" $ do
    letter <- satisfy isUpperHexDigit <* "/"
    Base64.decodeLetter4 letter ?? fail "Base64.decodeLetter4"

pScheme :: Parser Word2
pScheme = label "scheme" $
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
        char '^' *> skipSpace *> (AFloat   <$> double ) <+>
        char '=' *> skipSpace *> (AInteger <$> integer) <+>
        char '>' *> skipSpace *> (AUuid    <$> uuid'  ) <+>
        AString                            <$> string
    integer = signed decimal
    uuid'   = uuid Nothing (Just prevUuid) SameOpPrevUuid

parseAtom :: ByteStringL -> Either String Atom
parseAtom = parseOnlyL $ atom UUID.zero <* endOfInputEx

string :: Parser Text
string = do
    bs <- char '\'' *> content
    case Json.decodeStrict $ '"' `BSC.cons` (bs `BSC.snoc` '"') of
        Just s  -> pure s
        Nothing -> fail "bad string"
  where
    content = do
        chunk <- takeWhile $ \c -> c /= '\'' && c /= '\\'
        anyChar >>= \case
            '\'' -> pure chunk
            '\\' -> anyChar >>= \case
                '\'' -> (chunk <>) . BSC.cons '\'' <$> content
                c    -> (chunk <>) . BSC.cons '\\' . BSC.cons c <$> content
            _ -> fail "cannot happen"
        --  <* char '\''

parseString :: ByteStringL -> Either String Text
parseString = parseOnlyL $ string <* endOfInputEx

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
