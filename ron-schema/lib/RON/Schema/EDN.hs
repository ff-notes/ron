{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema.EDN (parseSchema) where

import           Control.Monad (unless)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, put)
import           Control.Monad.Trans (MonadTrans, lift)
import           Control.Monad.Trans.Identity (runIdentityT)
import           Data.Attoparsec.Lazy (Result (Done))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isSpace)
import           Data.EDN (Tagged (NoTag, Tagged), Value (List, Map, Symbol),
                           (.!=), (.:?))
import           Data.EDN.Encode (fromTagged)
import           Data.EDN.Parser (parseBSL)
import           Data.EDN.Types (EDNList, EDNMap)
import           Data.EDN.Types.Class (Parser, parseEither, typeMismatch)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextL
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as TextL

import           RON.Util (ByteStringL)

import           RON.Schema

newtype Env = Env {knownTypes :: Map Text RonType}
    deriving (Show)

startEnv :: Env
startEnv = Env
    { knownTypes = Map.fromList
        [ ("Boole",
            opaqueAtoms "Boole" OpaqueAnnotations{oaHaskellType = Just "Bool"})
        , ("Day",           day)
        , ("Integer",       TAtom TAInteger)
        , ("RgaString",     TObject $ TRga char)
        , ("String",        TAtom TAString)
        , ("VersionVector", TObject TVersionVector)
        ]
    }
  where
    char = opaqueAtoms "Char" OpaqueAnnotations{oaHaskellType = Just "Char"}
    day = opaqueAtoms_ "Day"

type Parser' = StateT Env Parser

parseDeclaration :: Tagged Value -> Parser' Declaration
parseDeclaration = withNoTag $ withList "declaration" $ \case
    func : args -> go =<< parseText "declaration name" func
      where
        go = \case
            "enum"       -> DEnum      <$> parseEnum      args
            "opaque"     -> DOpaque    <$> parseOpaque    args
            "struct_lww" -> DStructLww <$> parseStructLww args
            name         -> fail $ "unknown declaration " ++ Text.unpack name
    [] -> fail "empty declaration"

parseEnum :: EDNList -> Parser' TEnum
parseEnum code = do
    enum <- case code of
        name : items -> Enum
            <$> parseText "enum name" name
            <*> traverse (parseText "enum item") items
        [] -> fail
            "Expected declaration in the form\
            \ (enum <name:symbol> <item:symbol>...)"
    insertKnownType (enumName enum) (TComposite $ TEnum enum)
    pure enum

parseOpaque :: EDNList -> Parser' Opaque
parseOpaque code = do
    opaque <- case code of
        kind : name : annotations ->
            parseText "opaque kind" kind >>= \case
                "atoms"  -> go False
                "object" -> go True
                _        -> fail "opaque kind must be either atoms or object"
          where
            go isObject =
                Opaque
                    isObject
                    <$> parseText "opaque name" name
                    <*> parseAnnotations
            parseAnnotations = case annotations of
                [] -> pure defaultOpaqueAnnotations
                _  -> fail "opaque annotations are not implemented yet"
        _ -> fail
            "Expected declaration in the form\
            \ (opaque <kind:symbol> <name:symbol> <annotations>...)"
    insertKnownType (opaqueName opaque) (TOpaque opaque)
    pure opaque

insertKnownType :: Text -> RonType -> Parser' ()
insertKnownType name typ = do
    env@Env{knownTypes} <- get
    case Map.lookup name knownTypes of
        Nothing -> put env{knownTypes = Map.insert name typ knownTypes}
        Just _  -> fail $ "duplicate declaration of type " ++ Text.unpack name

parseStructLww :: EDNList -> Parser' StructLww
parseStructLww code = do
    struct <- case code of
        name : body -> do
            let (annotations, fields) = span isTagged body
            StructLww
                <$> parseText "struct_lww name" name
                <*> parseFields fields
                <*> parseAnnotations annotations
        [] -> fail
            "Expected declaration in the form\
            \ (struct_lww <name:symbol> <annotations>... <fields>...)"
    insertKnownType (structName struct) (TObject $ TStructLww struct)
    pure struct

  where

    parseFields = \case
        [] -> pure mempty
        nameAsTagged : typeAsTagged : cont -> do
            name <- parseText "struct_lww field name" nameAsTagged
            typ  <- parseType typeAsTagged
            Map.insert name (Field typ) <$> parseFields cont
        [f] -> fail $ "field " ++ showEdn f ++ " must have type"

    parseAnnotations annTaggedValues = do
        annValues <- traverse unwrapTag annTaggedValues
        case lookup "haskell" annValues of
            Nothing -> pure defaultStructAnnotations
            Just annValue ->
                withMap "struct_lww haskell annotations map" go annValue
      where
        unwrapTag = \case
            Tagged value prefix tag -> let
                name | BS.null prefix = tag | otherwise = prefix <> "/" <> tag
                in pure (name, value)
            NoTag _ -> fail "annotation must be a tagged value"
        go m = lift $
            StructAnnotations
            <$>  m .:? Symbol "" "field_prefix" .!= ""
            <*> (m .:? Symbol "" "field_case" >>= traverse parseCaseTransform)

parseCaseTransform :: Tagged Value -> Parser CaseTransform
parseCaseTransform v =
    runIdentityT (parseText "case transformation" v) >>= \case
        "title" -> pure TitleCase
        _       -> fail "unknown case transformation"

parseSchema :: Monad m => String -> m Schema
parseSchema string = either fail pure $ do
    values <- parseEdnStream $ encodeUtf8L string
    parseEither ((`evalStateT` startEnv) . traverse parseDeclaration) values

parseEdnStream :: ByteStringL -> Either String EDNList
parseEdnStream input
    | BSLC.all isSpace input = pure []
    | otherwise              =
        case parseBSL input of
            Done rest value -> (value :) <$> parseEdnStream rest
            failure         -> Left $ show failure

parseType :: Tagged Value -> Parser' RonType
parseType = withNoTag $ \case
    Symbol "" name ->
        gets (Map.lookup (Text.decodeUtf8 name) . knownTypes) >>= \case
            Nothing  -> fail $ "unknown type " ++ decodeUtf8 name
            Just typ -> pure typ
    Symbol _ _ -> fail "types must not be prefixed"
    List expr -> evalType expr
    value -> lift $ typeMismatch "type symbol or expression" value

evalType :: EDNList -> Parser' RonType
evalType = \case
    []  -> fail "empty type expression"
    [a] -> parseType a
    func : args -> applyType func =<< traverse parseType args

applyType :: Tagged Value -> [RonType] -> Parser' RonType
applyType func args = parseText "parametric type" func >>= go
  where

    go = \case
        "Option" -> apply "Option" $ TComposite . TOption
        "ORSet"  -> apply "ORSet"  $ TObject    . TORSet
        name     -> fail $ "unknown parametric type " ++ Text.unpack name

    apply name wrapper = case args of
        [a] -> pure $ wrapper a
        _  -> fail $ name ++ " expects 1 argument, got " ++ show (length args)

-- * Parser helpers

withNoPrefix
    :: Monad m
    => String -> (ByteString -> m a) -> ByteString -> ByteString -> m a
withNoPrefix ctx f prefix name = do
    unless (prefix == "") $ fail $ ctx ++ ": empty prefix expected"
    f name

withList :: String -> (EDNList -> Parser' a) -> Value -> Parser' a
withList expected f = \case
    List list -> f list
    value     -> lift $ typeMismatch expected value

withMap :: String -> (EDNMap -> Parser' a) -> Value -> Parser' a
withMap expected f = \case
    Map m -> f m
    value -> lift $ typeMismatch expected value

withNoTag :: Monad m => (Value -> m a) -> Tagged Value -> m a
withNoTag f = \case
    NoTag value         -> f value
    Tagged _ prefix tag -> fail
        $ "when expecting a non-tagged value, encountered tag "
        ++ decodeUtf8 prefix ++ "/" ++ decodeUtf8 tag ++ " instead"

withSymbol
    :: MonadTrans t
    => String -> (ByteString -> ByteString -> t Parser a) -> Value -> t Parser a
withSymbol expected f = \case
    Symbol prefix symbol -> f prefix symbol
    value                -> lift $ typeMismatch expected value

parseText
    :: (MonadTrans t, Monad (t Parser))
    => String -> Tagged Value -> t Parser Text
parseText name =
    withNoTag $ withSymbol (name ++ " symbol") $ withNoPrefix name $
    pure . Text.decodeUtf8

-- * ByteString helpers

decodeUtf8 :: ByteString -> String
decodeUtf8 = Text.unpack . Text.decodeUtf8

encodeUtf8L :: String -> ByteStringL
encodeUtf8L = TextL.encodeUtf8 . TextL.pack

isTagged :: Tagged a -> Bool
isTagged = \case
    NoTag {} -> False
    Tagged{} -> True

showEdn :: Tagged Value -> String
showEdn = TextL.unpack . toLazyText . fromTagged
