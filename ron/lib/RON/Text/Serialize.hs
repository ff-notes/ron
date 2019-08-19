{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | RON-Text serialization
module RON.Text.Serialize
  ( serializeAtom,
    serializeObject,
    serializeRawOp,
    serializeStateFrame,
    serializeString,
    serializeUuid,
    serializeWireFrame,
    serializeWireFrames
    )
where

import Control.Monad.State.Strict (state)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (elem)
import qualified Data.Map.Strict as Map
import RON.Prelude
import RON.Text.Serialize.UUID
  ( serializeUuid,
    serializeUuidAtom,
    serializeUuidKey
    )
import RON.Types
  ( Atom (AFloat, AInteger, AString, AUuid),
    ClosedOp (..),
    ObjectFrame (..),
    Op (..),
    Payload,
    StateFrame,
    WireChunk (Closed, Query, Value),
    WireFrame,
    WireReducedChunk (..),
    WireStateChunk (..)
    )
import RON.UUID (UUID, zero)
import RON.Util (ByteStringL)

-- | Serialize a common frame
serializeWireFrame :: WireFrame -> ByteStringL
serializeWireFrame chunks =
  (`BSLC.snoc` '.')
    . fold
    . (`evalState` opZero)
    $ traverse serializeChunk chunks

-- | Serialize a sequence of common frames
serializeWireFrames :: [WireFrame] -> ByteStringL
serializeWireFrames = foldMap serializeWireFrame

-- | Serialize a common chunk
serializeChunk :: WireChunk -> State ClosedOp ByteStringL
serializeChunk = \case
  Closed op -> (<> " ;\n") <$> serializeClosedOpZip op
  Value chunk -> serializeReducedChunk False chunk
  Query chunk -> serializeReducedChunk True chunk

-- | Serialize a reduced chunk
serializeReducedChunk :: Bool -> WireReducedChunk -> State ClosedOp ByteStringL
serializeReducedChunk isQuery WireReducedChunk {wrcHeader, wrcBody} =
  BSLC.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
      h <- serializeClosedOpZip wrcHeader
      pure $ BSLC.unwords [h, if isQuery then "?" else "!"]
    serializeBody = state $ \ClosedOp {op = opBefore, ..} ->
      let (body, opAfter) =
            (`runState` opBefore)
              $ for wrcBody
              $ fmap ("\t" <>)
              . serializeReducedOpZip objectId
       in (body, ClosedOp {op = opAfter, ..})

-- | Serialize a context-free raw op
serializeRawOp :: ClosedOp -> ByteStringL
serializeRawOp op = evalState (serializeClosedOpZip op) opZero

-- | Serialize a raw op with compression in stream context
serializeClosedOpZip :: ClosedOp -> State ClosedOp ByteStringL
serializeClosedOpZip this = state $ \prev ->
  let prev' = op prev
      typ = serializeUuidKey (reducerId prev) zero (reducerId this)
      obj = serializeUuidKey (objectId prev) (reducerId this) (objectId this)
      evt = serializeUuidKey (opId prev') (objectId this) (opId this')
      ref = serializeUuidKey (refId prev') (opId this') (refId this')
      payloadAtoms = serializePayload (objectId this) (payload this')
   in ( BSLC.unwords
          $ key '*' typ
          ++ key '#' obj
          ++ key '@' evt
          ++ key ':' ref
          ++ [payloadAtoms | not $ BSL.null payloadAtoms],
        this
        )
  where
    this' = op this
    key c u = [BSLC.cons c u | not $ BSL.null u]

-- | Serialize a reduced op with compression in stream context
serializeReducedOpZip
  :: UUID -- ^ enclosing object
  -> Op
  -> State Op ByteStringL
serializeReducedOpZip opObject this = state $ \prev ->
  let evt = serializeUuidKey (opId prev) opObject (opId this)
      ref = serializeUuidKey (refId prev) (opId this) (refId this)
      payloadAtoms = serializePayload opObject (payload this)
      keys
        | BSL.null evt && BSL.null ref = ["@"]
        | otherwise = key '@' evt ++ key ':' ref
      op = keys ++ [payloadAtoms | not $ BSL.null payloadAtoms]
   in (BSLC.unwords op, this)
  where
    key c u = [c `BSLC.cons` u | not $ BSL.null u]

-- | Serialize a context-free atom
serializeAtom :: Atom -> ByteStringL
serializeAtom a = evalState (serializeAtomZip a) zero

-- | Serialize an atom with compression for UUID in stream context
serializeAtomZip :: Atom -> State UUID ByteStringL
serializeAtomZip = \case
  AFloat f -> pure $ serializeFloatAtom f
  AInteger i -> pure $ serializeIntegerAtom i
  AString s -> pure $ serializeString s
  AUuid u -> serializeUuidAtom' u

-- | Serialize a float atom.
-- If unambiguous, i.e. contains a '.' or an 'e'/'E', the prefix '^' is skipped.
serializeFloatAtom :: Double -> ByteStringL
serializeFloatAtom float
  | isDistinguishableFromUuid = asByteString
  | otherwise = '^' `BSLC.cons` asByteString
  where
    asString = show float
    isDistinguishableFromUuid =
      '.' `elem` asString || 'e' `elem` asString || 'E' `elem` asString
    asByteString = BSLC.pack asString

-- | Serialize an integer atom.
-- Since integers are always unambiguous, the prefix '=' is always skipped.
serializeIntegerAtom :: Int64 -> ByteStringL
serializeIntegerAtom = BSLC.pack . show

-- | Serialize a string atom
serializeString :: Text -> ByteStringL
serializeString =
  wrapSingleQuotes . escapeApostrophe . stripDoubleQuotes . Json.encode
  where
    wrapSingleQuotes = (`BSLC.snoc` '\'') . BSLC.cons '\''
    stripDoubleQuotes = BSL.init . BSL.tail
    escapeApostrophe s
      | BSL.null s2 = s1
      | otherwise = s1 <> "\\'" <> escapeApostrophe (BSL.tail s2)
      where
        (s1, s2) = BSLC.break (== '\'') s

serializeUuidAtom' :: UUID -> State UUID ByteStringL
serializeUuidAtom' u =
  -- TODO(2019-08-19, cblp): Check if uuid can be unambiguously serialized and
  -- if so, skip the prefix.
  state $ \prev -> (BSLC.cons '>' $ serializeUuidAtom prev u, u)

-- | Serialize a payload in stream context
serializePayload
  :: UUID -- ^ previous UUID (default is 'zero')
  -> Payload
  -> ByteStringL
serializePayload prev =
  BSLC.unwords . (`evalState` prev) . traverse serializeAtomZip

-- | Serialize a state frame
serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame = serializeWireFrame . map wrapChunk . Map.assocs
  where
    wrapChunk (objectId, WireStateChunk {stateType, stateBody}) =
      Value WireReducedChunk
        { wrcHeader = opZero {reducerId = stateType, objectId},
          wrcBody = stateBody
          }

-- | Serialize an object. Return object id that must be stored separately.
serializeObject :: ObjectFrame a -> (UUID, ByteStringL)
serializeObject (ObjectFrame oid frame) = (oid, serializeStateFrame frame)

opZero :: ClosedOp
opZero = ClosedOp
  { reducerId = zero,
    objectId = zero,
    op = Op {opId = zero, refId = zero, payload = []}
    }
