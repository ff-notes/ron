{-# LANGUAGE OverloadedStrings #-}

module Attoparsec.Extra
    ( module Attoparsec
    , endOfInputEx
    , getPos
    , isSuccessful
    , label
    , label'
    , manyTillEnd
    , parseOnlyL
    , someTillEnd
    , takeAtMost
    , takeL
    , withInputSize
    , (??)
    ) where

import           Internal.Prelude

import qualified Data.Attoparsec.Internal.Types as Internal
import           Data.Attoparsec.Lazy as Attoparsec
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)

parseOnlyL :: Parser a -> ByteStringL -> Either String a
parseOnlyL p = parseOnly p . toStrict

-- | 'Attoparsec.take' adapter to 'ByteStringL'
takeL :: Int -> Parser ByteStringL
takeL = fmap fromStrict . Attoparsec.take

getPos :: Parser Int
getPos =
    Internal.Parser $ \t pos more _ suc -> suc t pos more $ Internal.fromPos pos

withInputSize :: Parser a -> Parser (Int, a)
withInputSize p = do
    posBefore <- getPos
    r <- p
    posAfter <- getPos
    pure (posAfter - posBefore, r)

label :: String -> Parser a -> Parser a
label = flip (<?>)

label' :: String -> Parser a -> Parser a
label' name p = do
    pos <- getPos
    label (name ++ ':' : show pos) p

manyTillEnd :: Parser a -> Parser [a]
manyTillEnd p = liftA2 (:) p (someTillEnd p)

someTillEnd :: Parser a -> Parser [a]
someTillEnd p = do
    r <- p
    weAreAtEnd <- atEnd
    if weAreAtEnd then
        pure [r]
    else
        someTillEnd p

-- | Variant of 'endOfInput' with a more debuggable message.
endOfInputEx :: Parser ()
endOfInputEx = do
    weAreAtEnd <- atEnd
    unless weAreAtEnd $ do
        pos <- getPos
        rest <- takeAtMost 11
        let cite
                | BS.length rest < 11 = rest
                | otherwise           = BS.take 10 rest <> "..."
        fail $ show pos <> ": extra input: " <> show cite

takeAtMost :: Int -> Parser ByteString
takeAtMost limit = do
    pos0 <- getPos
    BS.pack <$> manyTill anyWord8 (checkLimit $ pos0 + limit)
  where
    checkLimit maxPos = do
        pos <- getPos
        guard (pos >= maxPos) <|> endOfInput

(??) :: Maybe a -> String -> Parser a
(??) a msg = maybe (fail msg) pure a

-- | Apply parser and check it is applied successfully.
-- Kinda opposite to 'guard'.
isSuccessful :: Alternative f => f a -> f Bool
isSuccessful p = p $> True <|> pure False
