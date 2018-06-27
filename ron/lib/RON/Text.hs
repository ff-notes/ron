{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text (parseFrames, serialize) where

import           Internal.Prelude

import           Attoparsec.Extra (Parser, endOfInput, label, option, parseOnly,
                                   satisfy)
import           Data.Attoparsec.ByteString.Char8 (anyChar, skipSpace,
                                                   takeWhile1)
import           Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (ord)
import           Data.Functor (($>))

import qualified RON.Base64 as Base64
import           RON.Types (Frame, Op (..))
import           RON.UUID (UUID (..), UuidFields (..))
import qualified RON.UUID as UUID

parseFrames :: ByteStringL -> Either String [Frame]
parseFrames =
    parseOnly ((skipSpace <* endOfInput $> []) <|> some pFrame) . BSL.toStrict

pFrame :: Parser Frame
pFrame = label "Frame" $ ("." $> []) <|> (some pOp <* optional ".")

pOp :: Parser Op
pOp = label "Op" $ do
    typ      <- label "type"     $ skipSpace *> "*" *> pUuid
    object   <- label "object"   $ skipSpace *> "#" *> pUuid
    event    <- label "event"    $ skipSpace *> "@" *> pUuid
    location <- label "location" $ skipSpace *> ":" *> pUuid
    pure Op{..}

data Base64Format = Ronic | Alien
    deriving (Eq, Show)

pUuid :: Parser UUID
pUuid = label "UUID" $ do
    (xFormat, x) <- do
        mvariety <- optional $ satisfy isUpperHexDigit <* "/"
        (format, word0) <- pBase64word
        word <- case (format, mvariety) of
            (Ronic, Nothing) -> pure $ '0' `BSC.cons` word0
            (Ronic, Just v ) -> pure $  v   `BS.cons` word0
            (Alien, Nothing) -> pure word0
            (Alien, Just _ ) -> "mixing RON variety with alien UUID"
        case Base64.decode64 word of
            Nothing -> fail "Base64 decoding error"
            Just w  -> pure (format, w)
    (yFormat, y) <- option (Ronic, 0) $ do
        skipSpace
        mscheme <- optional pScheme
        (format, word) <- pBase64word
        mw <- case (format, mscheme) of
            (Ronic, Nothing) -> pure $ Base64.decode60 word
            (Ronic, Just scheme) -> pure $
                ((fromIntegral scheme `shiftL` 60) .|.) <$> Base64.decode60 word
            (Alien, Nothing) -> pure $ Base64.decode64 word
            (Alien, Just _) -> fail "mixing RON scheme with alien UUID"
        case mw of
            Nothing -> fail "Base64 decoding error"
            Just w  -> pure (format, w)
    if xFormat == yFormat then
        pure $ UUID x y
    else
        fail $ "mixing UUID formats " ++ show (xFormat, yFormat)

pBase64word :: Parser (Base64Format, ByteString)
pBase64word = label "Base64 word" $ do
    word <- takeWhile1 Base64.isLetter
    case BS.length word of
        11         -> pure (Alien, word)
        n | n < 11 -> pure (Ronic, word)
        _          -> fail "too long Base64 word"

isUpperHexDigit :: Word8 -> Bool
isUpperHexDigit c =
    (fromIntegral (c - fromIntegral (ord '0')) :: Word) <= 9 ||
    (fromIntegral (c - fromIntegral (ord 'A')) :: Word) <= 5

pScheme :: Parser Word2
pScheme = label "scheme" $
    anyChar >>= \case
        '$' -> pure 0b00
        '%' -> pure 0b01
        '+' -> pure 0b10
        '-' -> pure 0b11
        _   -> fail "pScheme"

serialize :: Frame -> ByteStringL
serialize = (`BSLC.snoc` '.') . foldMap serializeOp

serializeOp :: Op -> ByteStringL
serializeOp Op{typ, object, event, location} =
    let t = serializeUuid typ
        o = serializeUuid object
        e = serializeUuid event
        r = serializeUuid location
    in "*" <> t <> " #" <> o <> " @" <> e <> " :" <> r

serializeUuid :: UUID -> ByteStringL
serializeUuid uuid@(UUID x y) = BSL.fromStrict $
    case variant of
        0b00 ->
            varietyPrefix               <>
            Base64.encode60 value       <>
            BSC.singleton schemeSymbol  <>
            Base64.encode60 origin
        _ -> Base64.encode64 x <> BSC.singleton ' ' <> Base64.encode64 y
  where
    UuidFields{variety, value, variant, scheme, origin} = UUID.split uuid
    schemeSymbol = case scheme of
        0b00 -> '$'
        0b01 -> '%'
        0b10 -> '+'
        _    -> '-'
    varietyPrefix = case variety of
        0 -> ""
        _ -> BS.singleton (Base64.encodeLetter variety) <> "/"
