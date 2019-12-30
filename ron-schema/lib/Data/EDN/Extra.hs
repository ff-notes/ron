{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Extra (
    SourcePos (..),
    decodeMultiDoc,
    isTagged,
    mkPos,
    parseList,
    parseSymbol',
    withNoPrefix,
    withSymbol',
) where

import           RON.Prelude

import qualified Data.EDN.AST.Lexer as EdnAst
import qualified Data.EDN.AST.Parser as EdnAst
import qualified Data.EDN.AST.Types as EdnAst
import           Data.EDN.Class.Parser (Parser)
import           Text.Megaparsec (SourcePos (..), mkPos)
import qualified Text.Megaparsec as Mpc

import           Data.EDN (EDNList, FromEDN, Tagged (NoTag, Tagged),
                           TaggedValue, Value (List), parseEDNv, withNoTag,
                           withSymbol)

withNoPrefix :: MonadFail m => (Text -> m a) -> Text -> Text -> m a
withNoPrefix f prefix name = case prefix of
    "" -> f name
    _  -> fail "Expected no prefix"

withSymbol' :: (Text -> Parser a) -> TaggedValue -> Parser a
withSymbol' = withNoTag . withSymbol . withNoPrefix

parseSymbol' :: TaggedValue -> Parser Text
parseSymbol' = withSymbol' pure

isTagged :: Tagged t a -> Bool
isTagged = \case
    NoTag {} -> False
    Tagged{} -> True

parseMultiDoc :: EdnAst.Parser [TaggedValue]
parseMultiDoc = EdnAst.dropWS *> many EdnAst.parseTagged <* Mpc.eof

decodeMultiDoc :: MonadFail m => SourcePos -> Text -> m [TaggedValue]
decodeMultiDoc sourcePos input =
    either (fail . Mpc.errorBundlePretty) pure
    $ snd
    $ Mpc.runParser' parseMultiDoc initState
  where
    initState = Mpc.State{
        stateInput    = input,
        stateOffset   = 0,
        statePosState = Mpc.PosState{
            pstateInput      = input,
            pstateOffset     = 0,
            pstateSourcePos  = sourcePos,
            pstateTabWidth   = Mpc.defaultTabWidth,
            pstateLinePrefix = ""
        }
    }

parseList :: FromEDN a => EDNList -> Parser a
parseList = parseEDNv . List
