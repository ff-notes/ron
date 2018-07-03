{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
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

import           Attoparsec.Extra (Parser, label, option, parseWhole, satisfy)
import           Data.Attoparsec.ByteString.Char8 (anyChar, char, skipSpace,
                                                   takeWhile1)
import           Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.Functor (($>))

import qualified RON.Base64 as Base64
import           RON.Types (Frame, Op (..))
import           RON.UUID (UUID (..))

parseFrame :: ByteStringL -> Either String Frame
parseFrame = parseWhole $ frameBody <* optional endOfFrame <* skipSpace

parseFrames :: ByteStringL -> Either String [Frame]
parseFrames = parseWhole $ many frame
  where
    frame = do
        ops <- frameBody
        eof <- optional endOfFrame
        guard $ not (null ops) || isJust eof
        pure ops

parseOp :: ByteStringL -> Either String Op
parseOp = parseWhole $ op <* skipSpace

parseUuid :: ByteStringL -> Either String UUID
parseUuid = parseWhole uuid

endOfFrame :: Parser ()
endOfFrame = label "end of frame" $ void $ skipSpace *> char '.'

frameBody :: Parser Frame
frameBody = label "Frame body" $ many op

frameInStream :: Parser Frame
frameInStream = label "Frame in stream" $ frameBody <* endOfFrame

op :: Parser Op
op = label "Op" $ do
    opType     <- header "type"     "*" Nothing
    opObject   <- header "object"   "#" (Just opType)
    opEvent    <- header "event"    "@" (Just opObject)
    opLocation <- header "location" ":" (Just opEvent)
    -- TODO: location is available as the previous-same-op UUID in payload
    payload
    pure Op{..}
  where
    header name signal mPrevSameOp =
        label name $ skipSpace *> signal *> uuidTerm mPrevSameOp

uuidTerm
    :: Maybe UUID
        -- ^ previous UUID of the same op
        -- (e.g. event id against same op's object id)
    -> Parser UUID
uuidTerm = \case
    Nothing         -> uuid
    Just prevSameOp -> ("`" $> prevSameOp) <|> uuid

data Base64Format = Ronic | Long
    deriving (Eq, Show)

uuid :: Parser UUID
uuid = label "UUID" $ long <|> ronic
  where
    long = label "long" $ do
        xy <- takeWhile1 Base64.isLetter
        guard $ BS.length xy == 22
        maybe (fail "Base64 decoding error") pure $
            UUID
                <$> Base64.decode64 (BS.take 11 xy)
                <*> Base64.decode64 (BS.drop 11 xy)
    ronic = label "ronic" $ do
        x <- do
            mvariety <- optional $ satisfy isUpperHexDigit <* "/"
            (format, word0) <- pBase64word
            word <- case (format, mvariety) of
                (Ronic, Nothing) -> pure $ '0' `BSC.cons` word0
                (Ronic, Just v ) -> pure $  v   `BS.cons` word0
                (Long,  Nothing) -> pure word0
                (Long,  Just _ ) -> "mixing RON variety with long UUID"
            case Base64.decode64 word of
                Nothing -> fail "Base64 decoding error"
                Just w  -> pure w
        y <- option 0 $ do
            mscheme <- (Just <$> scheme) <|> (skipSpace $> Nothing)
            (format, word) <- pBase64word
            mw <- case (format, mscheme) of
                (Ronic, Nothing   ) -> pure $ Base64.decode60 word
                (Ronic, Just schem) -> pure $
                    ((fromIntegral schem `shiftL` 60) .|.) <$>
                    Base64.decode60 word
                (Long, Nothing) -> pure $ Base64.decode64 word
                (Long, Just _ ) -> fail "mixing RON scheme with long UUID"
            case mw of
                Nothing -> fail "Base64 decoding error"
                Just w  -> pure w
        pure $ UUID x y

pBase64word :: Parser (Base64Format, ByteString)
pBase64word = label "Base64 word" $ do
    word <- takeWhile1 Base64.isLetter
    case BS.length word of
        11         -> pure (Long, word)
        n | n < 11 -> pure (Ronic, word)
        _          -> fail "too long Base64 word"

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

payload :: Parser ()
payload = label "payload" $ void $ skipSpace *> char '!'
