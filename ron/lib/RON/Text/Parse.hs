{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Parse
    ( frameBody
    , frameInStream
    , parseFrame
    , parseFrames
    , parseOp
    , parseUuid
    ) where

import           Internal.Prelude

import           Attoparsec.Extra (Parser, endOfInputEx, failWith, isSuccessful,
                                   label, option, parseOnlyL, satisfy)
import           Data.Attoparsec.ByteString.Char8 (anyChar, char, digit,
                                                   peekChar, skipSpace,
                                                   takeWhile1)
import qualified Data.Attoparsec.ByteString.Char8 as AttoparsecChar
import           Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.Functor (($>))

import qualified RON.Base64 as Base64
import           RON.Types (Atom (..), Frame, Op (..), UUID (..))

parseFrame :: ByteStringL -> Either String Frame
parseFrame = parseOnlyL frameBodyToEnd

parseFrames :: ByteStringL -> Either String [Frame]
parseFrames = parseOnlyL $ many frame <* endOfInputEx
  where
    frame = do
        ops <- frameBody
        eof <- optional endOfFrame
        guard $ not (null ops) || isJust eof
        pure ops

parseOp :: ByteStringL -> Either String Op
parseOp = parseOnlyL $ fst <$> opStart <* skipSpace <* endOfInputEx

parseUuid :: ByteStringL -> Either String UUID
parseUuid = parseOnlyL $ uuid <* endOfInputEx

endOfFrame :: Parser ()
endOfFrame = label "end of frame" $ void $ skipSpace *> char '.'

frameBody :: Parser Frame
frameBody = label "Frame body" $ goStart <|> stop
  where
    goStart = do
        (x, u) <- opStart
        xs <- go (x, u) <|> stop
        pure $ x : xs
    go prev = do
        (x, u) <- op prev
        xs <- go (x, u) <|> stop
        pure $ x : xs
    stop = pure []

frameBodyToEnd :: Parser Frame
frameBodyToEnd = label "Frame body to end" $ stop <|> goStart
  where
    goStart = do
        (x, u) <- opStart
        xs <- stop <|> go (x, u)
        pure $ x : xs
    go prev = do
        (x, u) <- op prev
        xs <- stop <|> go (x, u)
        pure $ x : xs
    stop = optional endOfFrame *> skipSpace *> endOfInputEx $> []

frameInStream :: Parser Frame
frameInStream = label "Frame in stream" $ frameBody <* endOfFrame

opStart :: Parser (Op, UUID)
opStart = label "Op-start" $ do
    opType     <- keyStart "type"     '*'
    opObject   <- key      "object"   '#' opType
    opEvent    <- key      "event"    '@' opObject
    opLocation <- key      "location" ':' opEvent
    opPayload  <- payload
    pure (Op{..}, opLocation)
  where
    keyStart name keyChar =
        label name $ skipSpace *> label "key character" (char keyChar) *> uuid
    key name keyChar prev = label name $
        skipSpace *>
        label "key character" (char keyChar) *>
        (uuidCompressed prev <|> uuid)

op :: (Op, UUID) -> Parser (Op, UUID)
op (prevOp, prevUuid) = label "Op" $ do
    (hasTyp, opType)     <- key "type"     '*' opType     prevUuid
    (hasObj, opObject)   <- key "object"   '#' opObject   opType
    (hasEvt, opEvent)    <- key "event"    '@' opEvent    opObject
    (hasLoc, opLocation) <- key "location" ':' opLocation opEvent
    unless (hasTyp || hasObj || hasEvt || hasLoc) $ fail "no key found"
    opPayload <- payload
    pure (Op{..}, opLocation)
  where
    key name keyChar f prev = label name $ do
        skipSpace
        isKeyPresent <- isSuccessful $ char keyChar
        if isKeyPresent then do
            u <- uuidCompressed prev <|> uuid
            pure (True, u)
        else
            -- no key => use previous key
            pure (False, f prevOp)

uuidCompressed
    :: UUID
        -- ^ previous UUID of the same op
        -- (e.g. event id against same op's object id)
    -> Parser UUID
uuidCompressed u = label "UUID-compressed" $
    anyChar >>= \case
        '`' -> pure u
        c   -> fail $ "unsupported compression " ++ show c

data Base64Format = Ronic | Long
    deriving (Eq, Show)

uuid :: Parser UUID
uuid = label "UUID" $ uuidAsBase64DoubleWord <|> uuidRon

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
        failWith "Base64 decoding error" $ Base64.decode64 word
    y <- option 0 $ do
        mscheme <- Just <$> scheme <|> skipSpace $> Nothing
        (format, word) <- base64word
        mw <- case (format, mscheme) of
            (Ronic, Nothing   ) -> pure $ Base64.decode60 word
            (Ronic, Just schem) -> pure $
                ((fromIntegral schem `shiftL` 60) .|.) <$> Base64.decode60 word
            (Long, Nothing) -> pure $ Base64.decode64 word
            (Long, Just _ ) -> fail "mixing RON scheme with long UUID"
        failWith "Base64 decoding error" mw
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
    word <- AttoparsecChar.takeWhile Base64.isLetter
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
        '$' -> pure 0b00
        '%' -> pure 0b01
        '+' -> pure 0b10
        '-' -> pure 0b11
        _   -> fail "not a scheme"

payload :: Parser [Atom]
payload = label "payload" $ many atom

atom :: Parser Atom
atom = skipSpace *> atom'
  where
    atom' =
        "=" *> skipSpace *> (AInteger <$> integer) <|>
        ">" *> skipSpace *> (AUuid    <$> uuid   )
    integer = read <$> (maybe id (:) <$> optional (char '-') <*> some digit)
