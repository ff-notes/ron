{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Text.Serialize
    ( serializeAtom
    , serializeFrame
    , serializeFrames
    , serializeObject
    , serializeRawOp
    , serializeStateFrame
    , serializeString
    , serializeUuid
    ) where

import           RON.Internal.Prelude

import           Control.Monad.State.Strict (State, evalState, runState, state)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Search as BSL
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Text.Common (opZero)
import           RON.Text.Serialize.UUID (serializeUuid, serializeUuidAtom,
                                          serializeUuidKey)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            Object (..), Op (..), RawOp (..), StateChunk (..),
                            StateFrame, WireChunk (Query, Raw, Value),
                            WireFrame, WireReducedChunk (..))
import           RON.UUID (UUID, zero)

serializeFrame :: WireFrame -> ByteStringL
serializeFrame chunks
    = (`BSLC.snoc` '.')
    . mconcat
    . (`evalState` opZero)
    $ traverse serializeChunk chunks

serializeFrames :: [WireFrame] -> ByteStringL
serializeFrames = foldMap serializeFrame

serializeChunk :: WireChunk -> State RawOp ByteStringL
serializeChunk = \case
    Raw op      -> (<> " ;\n") <$> serializeRawOpZip op
    Value chunk -> serializeReducedChunk False chunk
    Query chunk -> serializeReducedChunk True  chunk

serializeReducedChunk :: Bool -> WireReducedChunk -> State RawOp ByteStringL
serializeReducedChunk isQuery WireReducedChunk{wrcHeader, wrcBody} =
    BSLC.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
        h <- serializeRawOpZip wrcHeader
        pure $ BSLC.unwords [h, if isQuery then "?" else "!"]
    serializeBody = state $ \RawOp{op = opBefore, ..} -> let
        (body, opAfter) =
            (`runState` opBefore) $
            for wrcBody $ \op -> do
                o <- serializeReducedOpZip opObject op
                pure $ "\t" <> BSLC.unwords [o, ","]
        in
        ( body
        , RawOp{op = opAfter, ..}
        )

serializeRawOp :: RawOp -> ByteStringL
serializeRawOp op = evalState (serializeRawOpZip op) opZero

serializeRawOpZip :: RawOp -> State RawOp ByteStringL
serializeRawOpZip this = state $ \prev -> let
    prev' = op prev
    typ = serializeUuidKey (opType   prev)  zero             (opType   this)
    obj = serializeUuidKey (opObject prev)  (opType   this)  (opObject this)
    evt = serializeUuidKey (opEvent  prev') (opObject this)  (opEvent  this')
    loc = serializeUuidKey (opRef    prev') (opEvent  this') (opRef    this')
    payload = serializePayload (opObject this) (opPayload this')
    in
    ( BSLC.unwords
        $   key '*' typ
        ++  key '#' obj
        ++  key '@' evt
        ++  key ':' loc
        ++  [payload | not $ BSL.null payload]
    , this
    )
  where
    this' = op this
    key c u = [BSLC.cons c u | not $ BSL.null u]

serializeReducedOpZip :: UUID -> Op -> State Op ByteStringL
serializeReducedOpZip opObject this = state $ \prev -> let
    evt = serializeUuidKey (opEvent  prev) opObject       (opEvent this)
    loc = serializeUuidKey (opRef    prev) (opEvent this) (opRef   this)
    payload = serializePayload opObject (opPayload this)
    in
    ( BSLC.unwords $
        key '@' evt ++ key ':' loc ++ [payload | not $ BSL.null payload]
    , this
    )
  where
    key c u = [BSLC.cons c u | not $ BSL.null u]

serializeAtom :: Atom -> ByteStringL
serializeAtom a = evalState (serializeAtomZip a) zero

serializeAtomZip :: Atom -> State UUID ByteStringL
serializeAtomZip = \case
    AFloat   f -> pure $ BSLC.cons '^' $ BSLC.pack (show f)
    AInteger i -> pure $ BSLC.cons '=' $ BSLC.pack (show i)
    AString  s -> pure $ serializeString s
    AUuid    u ->
        state $ \prev -> (BSLC.cons '>' $ serializeUuidAtom prev u, u)

serializeString :: Text -> ByteStringL
serializeString =
    fixQuotes . BSL.replace "'" ("\\'" :: ByteString) . Json.encode
  where
    fixQuotes = (`BSLC.snoc` '\'') . BSLC.cons '\'' . BSL.init . BSL.tail

serializePayload :: UUID -> [Atom] -> ByteStringL
serializePayload prev =
    BSLC.unwords . (`evalState` prev) . traverse serializeAtomZip

serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame = serializeFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value WireReducedChunk{..}
      where
        wrcHeader = RawOp{op = Op{opRef = zero, opPayload = [], ..}, ..}
        wrcBody = stateBody
        opEvent = stateVersion

serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object oid frame) = (oid, serializeStateFrame frame)
