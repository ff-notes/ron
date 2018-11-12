{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | RON-Text parsing
module RON.Text.Parse
    ( parseAtom
    , parseObject
    , parseOp
    , parseStateFrame
    , parseString
    , parseUuid
    , parseUuidKey
    , parseUuidAtom
    , parseWireFrame
    , parseWireFrames
    ) where

import           Prelude hiding (takeWhile)
import           RON.Internal.Prelude

import           Attoparsec.Extra (Parser, char, endOfInputEx, isSuccessful,
                                   label, manyTill, parseOnlyL, satisfy, (<+>),
                                   (??))
import qualified Data.Aeson as Json
import           Data.Attoparsec.ByteString (takeWhile1)
import           Data.Attoparsec.ByteString.Char8 (anyChar, decimal, double,
                                                   signed, skipSpace, takeWhile)
import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import qualified RON.Base64 as Base64
import           RON.Internal.Word (Word2, Word4, Word60, b00, b0000, b01, b10,
                                    b11, ls60, safeCast)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            Object (..), Op (..),
                            OpTerm (THeader, TQuery, TRaw, TReduced),
                            RawOp (..), StateChunk (..), StateFrame,
                            UUID (UUID), WireChunk (Query, Raw, Value),
                            WireFrame, WireReducedChunk (..))
import           RON.UUID (UuidFields (..))
import qualified RON.UUID as UUID

-- | Parse a common frame
parseWireFrame :: ByteStringL -> Either String WireFrame
parseWireFrame = parseOnlyL frame

chunksTill :: Parser () -> Parser [WireChunk]
chunksTill end = label "[WireChunk]" $ go opZero
  where
    go prev = do
        skipSpace
        atEnd <- isSuccessful end
        if atEnd then
            pure []
        else do
            (ch, lastOp) <- pChunk prev
            (ch :) <$> go lastOp

-- | Returns a chunk and the last op in it
pChunk :: RawOp -> Parser (WireChunk, RawOp)
pChunk prev = label "WireChunk" $ wireStateChunk prev <+> chunkRaw prev

chunkRaw :: RawOp -> Parser (WireChunk, RawOp)
chunkRaw prev = label "WireChunk-raw" $ do
    skipSpace
    (_, x) <- rawOp prev
    skipSpace
    void $ char ';'
    pure (Raw x, x)

-- | Returns a chunk and the last op (converted to raw) in it
wireStateChunk :: RawOp -> Parser (WireChunk, RawOp)
wireStateChunk prev = label "WireChunk-reduced" $ do
    (wrcHeader, isQuery) <- header prev
    let reducedOps y = do
            skipSpace
            (isNotEmpty, x) <- reducedOp (opObject wrcHeader) y
            t <- optional term
            unless (t == Just TReduced || isNothing t) $
                fail "reduced op may end with `,` only"
            unless (isNotEmpty || t == Just TReduced) $ fail "Empty reduced op"
            xs <- reducedOps x <|> stop
            pure $ x : xs
    wrcBody <- reducedOps (op wrcHeader) <|> stop
    let lastOp = case wrcBody of
            [] -> op wrcHeader
            _  -> last wrcBody
        wrap op = RawOp
            {opType = opType wrcHeader, opObject = opObject wrcHeader, op}
    pure ((if isQuery then Query else Value) WireReducedChunk{..}, wrap lastOp)
  where
    stop = pure []

frame :: Parser WireFrame
frame = label "WireFrame" $ chunksTill (endOfFrame <|> endOfInputEx)

-- | Parse a sequence of common frames
parseWireFrames :: ByteStringL -> Either String [WireFrame]
parseWireFrames = parseOnlyL $ manyTill frameInStream endOfInputEx

frameInStream :: Parser WireFrame
frameInStream = label "WireFrame-stream" $ chunksTill endOfFrame

-- | Parse a single context-free op
parseOp :: ByteStringL -> Either String RawOp
parseOp = parseOnlyL $ do
    (_, x) <- rawOp opZero <* skipSpace <* endOfInputEx
    pure x

-- | Parse a single context-free UUID
parseUuid :: ByteStringL -> Either String UUID
parseUuid = parseOnlyL $
    uuid UUID.zero UUID.zero PrevOpSameKey <* skipSpace <* endOfInputEx

-- | Parse a UUID in key position
parseUuidKey
    :: UUID  -- ^ same key in the previous op (default is 'UUID.zero')
    -> UUID  -- ^ previous key of the same op (default is 'UUID.zero')
    -> ByteStringL
    -> Either String UUID
parseUuidKey prevKey prev =
    parseOnlyL $ uuid prevKey prev PrevOpSameKey <* skipSpace <* endOfInputEx

-- | Parse a UUID in value (atom) position
parseUuidAtom
    :: UUID  -- ^ previous
    -> ByteStringL
    -> Either String UUID
parseUuidAtom prev = parseOnlyL $ uuidAtom prev <* skipSpace <* endOfInputEx

endOfFrame :: Parser ()
endOfFrame = label "end of frame" $ void $ skipSpace *> char '.'

rawOp :: RawOp -> Parser (Bool, RawOp)
rawOp prev = label "RawOp-cont" $ do
    (hasTyp, opType)   <- key "type"   '*' (opType   prev)  UUID.zero
    (hasObj, opObject) <- key "object" '#' (opObject prev)  opType
    (hasEvt, opEvent)  <- key "event"  '@' (opEvent  prev') opObject
    (hasLoc, opRef)    <- key "ref"    ':' (opRef    prev') opEvent
    opPayload <- payload opObject
    let op = Op{..}
    pure
        ( hasTyp || hasObj || hasEvt || hasLoc || not (null opPayload)
        , RawOp{..}
        )
  where
    prev' = op prev

reducedOp :: UUID -> Op -> Parser (Bool, Op)
reducedOp opObject prev = label "Op-cont" $ do
    (hasEvt, opEvent) <- key "event" '@' (opEvent prev) opObject
    (hasLoc, opRef)   <- key "ref"   ':' (opRef   prev) opEvent
    opPayload <- payload opObject
    let op = Op{..}
    pure (hasEvt || hasLoc || not (null opPayload), op)

key :: String -> Char -> UUID -> UUID -> Parser (Bool, UUID)
key name keyChar prevOpSameKey sameOpPrevUuid = label name $ do
    skipSpace
    isKeyPresent <- isSuccessful $ char keyChar
    if isKeyPresent then do
        u <- uuid prevOpSameKey sameOpPrevUuid PrevOpSameKey
        pure (True, u)
    else
        -- no key => use previous key
        pure (False, prevOpSameKey)

uuid :: UUID -> UUID -> UuidZipBase -> Parser UUID
uuid prevOpSameKey sameOpPrevUuid defaultZipBase = label "UUID" $
    uuid22 <+> uuid11 <+> uuidZip prevOpSameKey sameOpPrevUuid defaultZipBase

uuid11 :: Parser UUID
uuid11 = label "UUID-RON-11-letter-value" $ do
    rawX <- base64word 11
    guard $ BS.length rawX == 11
    x <- Base64.decode64 rawX ?? fail "Base64.decode64"
    skipSpace
    rawUuidVersion <- optional pUuidVersion
    rawOrigin <- optional $ base64word $ maybe 11 (const 10) rawUuidVersion
    y <- case (rawUuidVersion, BS.length <$> rawOrigin) of
        (Nothing, Just 11) ->
            case rawOrigin of
                Nothing     -> pure 0
                Just origin -> Base64.decode64 origin ?? fail "Base64.decode64"
        _ -> do
            origin <- case rawOrigin of
                Nothing     -> pure $ ls60 0
                Just origin -> Base64.decode60 origin ?? fail "Base64.decode60"
            pure $ UUID.buildY b00 (fromMaybe b00 rawUuidVersion) origin
    pure $ UUID x y

data UuidZipBase = PrevOpSameKey | SameOpPrevUuid

uuidZip :: UUID -> UUID -> UuidZipBase -> Parser UUID
uuidZip prevOpSameKey sameOpPrevUuid defaultZipBase = label "UUID-zip" $ do
    changeZipBase <- isSuccessful $ char '`'
    rawVariety <- optional pVariety
    rawReuseValue <- optional pReuse
    rawValue <- optional $ base64word60 $ 10 - fromMaybe 0 rawReuseValue
    skipSpace
    rawUuidVersion <- optional pUuidVersion
    rawReuseOrigin <- optional pReuse
    rawOrigin <- optional $ base64word60 $ 10 - fromMaybe 0 rawReuseOrigin

    let prev = UUID.split $ whichPrev changeZipBase
    let isSimple
            =   uuidVariant prev /= b00
            ||  (   not changeZipBase
                &&  isNothing rawReuseValue && isJust rawValue
                &&  isNothing rawReuseOrigin
                &&  (isNothing rawUuidVersion || isJust rawOrigin)
                )

    if isSimple then
        pure $ UUID.build UuidFields
            { uuidVariety = fromMaybe b0000    rawVariety
            , uuidValue   = fromMaybe (ls60 0) rawValue
            , uuidVariant = b00
            , uuidVersion = fromMaybe b00      rawUuidVersion
            , uuidOrigin  = fromMaybe (ls60 0) rawOrigin
            }
    else do
        uuidVariety <- pure $ fromMaybe (uuidVariety prev) rawVariety
        uuidValue <- pure $ reuse rawReuseValue rawValue (uuidValue prev)
        let uuidVariant = b00
        uuidVersion <- pure $ fromMaybe (uuidVersion prev) rawUuidVersion
        uuidOrigin <-
            pure $ reuse rawReuseOrigin rawOrigin (uuidOrigin prev)
        pure $ UUID.build UuidFields{..}
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

pUuidVersion :: Parser Word2
pUuidVersion = label "UUID-version" $
    anyChar >>= \case
        '$' -> pure b00
        '%' -> pure b01
        '+' -> pure b10
        '-' -> pure b11
        _   -> fail "not a UUID-version"

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
    uuid'   = uuidAtom prevUuid

uuidAtom :: UUID -> Parser UUID
uuidAtom prev = uuid UUID.zero prev SameOpPrevUuid

-- | Parse an atom
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

-- | Parse a string atom
parseString :: ByteStringL -> Either String Text
parseString = parseOnlyL $ string <* endOfInputEx

-- | Return 'RawOp' and 'chunkIsQuery'
header :: RawOp -> Parser (RawOp, Bool)
header prev = do
    (_, x) <- rawOp prev
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

-- | Parse a state frame
parseStateFrame :: ByteStringL -> Either String StateFrame
parseStateFrame = parseWireFrame >=> findObjects

-- | Parse a state frame as an object
parseObject :: UUID -> ByteStringL -> Either String (Object a)
parseObject oid bytes = Object oid <$> parseStateFrame bytes

-- | Extract object states from a common frame
findObjects :: WireFrame -> Either String StateFrame
findObjects = fmap Map.fromList . traverse loadBody where
    loadBody = \case
        Value WireReducedChunk{..} -> do
            let RawOp{..} = wrcHeader
            let Op{..} = op
            let stateVersion = opEvent
            let stateBody = wrcBody
            pure ((opType, opObject), StateChunk{..})
        _ -> Left "expected reduced chunk"

opZero :: RawOp
opZero = RawOp
    { opType   = UUID.zero
    , opObject = UUID.zero
    , op       = Op{opEvent = UUID.zero, opRef = UUID.zero, opPayload = []}
    }
