module Attoparsec.Extra
    ( module Attoparsec
    , label
    , parseWhole
    , takeL
    , withInputSize
    ) where

import           Internal.Prelude

import qualified Data.Attoparsec.Internal.Types as Internal
import           Data.Attoparsec.Lazy as Attoparsec
import           Data.ByteString.Lazy (fromStrict, toStrict)

parseWhole :: Parser a -> ByteStringL -> Either String a
parseWhole p = parseOnly (p <* endOfInput) . toStrict

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
