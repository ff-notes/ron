{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | RON-Text serialization
module RON.Text.Serialize (
    serializeAtom,
    serializeObject,
    serializeOp,
    serializeOpenOp,
    serializePayload,
    serializeRawOp,
    serializeStateFrame,
    serializeString,
    serializeUuid,
    serializeWireFrame,
    serializeWireFrames,
    uuidToString,
    uuidToText,
) where

import RON.Prelude hiding (elem)

import Control.Monad.State.Strict (state)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy.Char8 (cons, elem, snoc)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map

import RON.Text.Serialize.UUID (
    serializeUuid,
    serializeUuidAtom,
    serializeUuidKey,
    uuidToString,
    uuidToText,
 )
import RON.Types (
    Atom (AFloat, AInteger, AString, AUuid),
    ClosedOp (ClosedOp),
    ObjectFrame (ObjectFrame),
    Op (Op),
    Payload,
    StateFrame,
    WireChunk (Closed, Query, Value),
    WireFrame,
    WireReducedChunk (WireReducedChunk),
    WireStateChunk (WireStateChunk),
    objectId,
    reducerId,
 )
import RON.Types qualified
import RON.UUID (UUID, succValue, zero)

-- | Serialize a common frame
serializeWireFrame :: WireFrame -> ByteStringL
serializeWireFrame =
    (`snoc` '.') . fold . (`evalState` closedOpZero) . traverse serializeChunk

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
serializeReducedChunk isQuery WireReducedChunk{wrcHeader, wrcBody} =
    BSL.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
        h <- serializeClosedOpZip wrcHeader
        pure $ BSL.intercalate "\t" [h, if isQuery then "?" else "!"]
    serializeBody = state \ClosedOp{op = opBefore, ..} ->
        let (body, opAfter) =
                (`runState` opBefore) $
                    for wrcBody $
                        fmap ("\t" <>)
                            . serializeReducedOpZip objectId
         in (body, ClosedOp{op = opAfter, ..})

-- | Serialize a context-free raw op
serializeRawOp :: ClosedOp -> ByteStringL
serializeRawOp op = evalState (serializeClosedOpZip op) closedOpZero

-- | Serialize a raw op with compression in stream context
serializeClosedOpZip :: ClosedOp -> State ClosedOp ByteStringL
serializeClosedOpZip this = state \prev ->
    let typ = serializeUuidKey prev.reducerId zero this.reducerId
        obj = serializeUuidKey prev.objectId this.reducerId this.objectId
        evt = serializeUuidKey prev.op.opId this.objectId this.op.opId
        ref = serializeUuidKey prev.op.refId this.op.opId this.op.refId
        payloadAtoms = serializePayloadZip this.objectId this.op.payload
     in ( BSL.intercalate "\t" $
            key '*' typ
                ++ key '#' obj
                ++ key '@' evt
                ++ key ':' ref
                ++ [payloadAtoms | not $ BSL.null payloadAtoms]
        , this
        )
  where
    key c u = [c `cons` u | not $ BSL.null u]

-- | Serialize a reduced op with compression in stream context
serializeReducedOpZip ::
    -- | enclosing object
    UUID ->
    Op ->
    State Op ByteStringL
serializeReducedOpZip opObject this = state \prev ->
    let evt = serializeUuidKey prev.opId opObject this.opId
        ref = serializeUuidKey prev.refId this.opId this.refId
        payloadAtoms = serializePayloadZip opObject this.payload
        keys
            | BSL.null evt && BSL.null ref = ["@"]
            | otherwise = key '@' evt ++ key ':' ref
        op = keys ++ [payloadAtoms | not $ BSL.null payloadAtoms]
     in (BSL.intercalate "\t" op, this)
  where
    key c u = [c `cons` u | not $ BSL.null u]

serializeOp :: Op -> ByteStringL
serializeOp Op{opId, refId, payload} =
    BSL.intercalate
        "\t"
        [ '@' `cons` serializeUuid opId
        , ':' `cons` serializeUuid refId
        , serializePayloadZip opId payload
        ]

serializeOpenOp ::
    -- | Previous op id
    UUID ->
    -- | Current op
    Op ->
    ByteStringL
serializeOpenOp prevId Op{opId, refId, payload} =
    BSL.intercalate "\t" $ idS : refS : payloadS
  where
    idS
        | opId /= succValue prevId = '@' `cons` serializeUuid opId
        | otherwise = ""
    refS
        | refId /= prevId = ':' `cons` serializeUuid refId
        | otherwise = ""
    payloadS = [serializePayloadZip opId payload | not $ null payload]

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

{- | Serialize a float atom.
If unambiguous, i.e. contains a '.' or an 'e'/'E', the prefix '^' is skipped.
-}
serializeFloatAtom :: Double -> ByteStringL
serializeFloatAtom float
    | isDistinguishableFromUuid = bs
    | otherwise = '^' `cons` bs
  where
    isDistinguishableFromUuid = '.' `elem` bs || 'e' `elem` bs || 'E' `elem` bs
    bs = BSL.pack $ show float

{- | Serialize an integer atom.
Since integers are always unambiguous, the prefix '=' is always skipped.
-}
serializeIntegerAtom :: Int64 -> ByteStringL
serializeIntegerAtom = BSL.pack . show

-- | Serialize a string atom
serializeString :: Text -> ByteStringL
serializeString =
    wrapSingleQuotes . escapeApostrophe . stripDoubleQuotes . Json.encode
  where
    wrapSingleQuotes = (`snoc` '\'') . cons '\''
    stripDoubleQuotes = BSL.init . BSL.tail
    escapeApostrophe s
        | BSL.null s2 = s1
        | otherwise = s1 <> "\\'" <> escapeApostrophe (BSL.tail s2)
      where
        (s1, s2) = BSL.break (== '\'') s

serializeUuidAtom' :: UUID -> State UUID ByteStringL
serializeUuidAtom' u =
    -- TODO(2019-08-19, cblp): Check if uuid can be unambiguously serialized and
    -- if so, skip the prefix.
    state \prev -> (cons '>' $ serializeUuidAtom prev u, u)

-- | Serialize a payload in stream context
serializePayloadZip ::
    -- | previous UUID (default is 'zero')
    UUID ->
    Payload ->
    ByteStringL
serializePayloadZip prev =
    BSL.unwords . (`evalState` prev) . traverse serializeAtomZip

-- | Serialize an abstract payload
serializePayload :: Payload -> ByteStringL
serializePayload = serializePayloadZip zero

-- | Serialize a state frame
serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame = serializeWireFrame . map wrapChunk . Map.assocs
  where
    wrapChunk (objectId, WireStateChunk{stateType, stateBody}) =
        Value
            WireReducedChunk
                { wrcHeader = closedOpZero{reducerId = stateType, objectId}
                , wrcBody = stateBody
                }

-- | Serialize an object. Return object id that must be stored separately.
serializeObject :: ObjectFrame a -> (UUID, ByteStringL)
serializeObject (ObjectFrame oid frame) = (oid, serializeStateFrame frame)

closedOpZero :: ClosedOp
closedOpZero =
    ClosedOp
        { reducerId = zero
        , objectId = zero
        , op = Op{opId = zero, refId = zero, payload = []}
        }
