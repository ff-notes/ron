{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Extra (
    decodeMultiDoc,
    isTagged,
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
import           Language.Haskell.TH (Loc (Loc))
import qualified Language.Haskell.TH as TH
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

decodeMultiDoc :: MonadFail m => Loc -> Text -> m [TaggedValue]
decodeMultiDoc Loc{loc_start = (line, column), loc_filename} input
    = either (fail . Mpc.errorBundlePretty) pure
    $ snd
    $ Mpc.runParser' parseMultiDoc initState
  where
    initState = Mpc.State{
        stateInput    = input,
        stateOffset   = 0,
        statePosState = Mpc.PosState{
            pstateInput      = input,
            pstateOffset     = 0,
            pstateSourcePos  = Mpc.SourcePos{
                sourceName   = loc_filename,
                sourceLine   = Mpc.mkPos line,
                sourceColumn = Mpc.mkPos column
            },
            pstateTabWidth   = undefined,
            pstateLinePrefix = undefined
        }
    }

parseList :: FromEDN a => EDNList -> Parser a
parseList = parseEDNv . List
