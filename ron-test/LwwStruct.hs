{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module LwwStruct where

import           RON.Internal.Prelude

import           Control.Error (note)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (stateToChunk, typeName)
import           RON.Data.LWW (LwwPerField (..))
import qualified RON.Data.RGA as Prim
import           RON.Event (Clock, EpochEvent (..),
                            Naming (ApplicationSpecific), ReplicaId (..),
                            encodeEvent, fromEpochEvent, getEventUuid)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import           RON.Text (parseFrame, serializeFrame)
import           RON.Types (Atom (..), Chunk (Value), Frame, Op (..), Op' (..),
                            RChunk (..), UUID (..))
import           RON.UUID (zero)
import qualified RON.UUID as UUID

-- | (type, object) -> body
type Frame' = Map (UUID, UUID) [Op']

-- | name -> op
type LwwChunk = Map UUID Op'

data Object a = Object {- object id -} UUID {- value -} a
    deriving (Eq, Show)

objectId :: Object a -> UUID
objectId (Object oid _) = oid

data LwwField a = LwwField {- time -} UUID {- value -} a
    deriving (Eq, Show)

newtype RGA a =
    RGA [({- vertex id -} UUID, {- tombstone -} UUID, {- value -} a)]
    deriving (Eq, Show)

class Replicated value rdt where
    initialize :: Clock m => value -> m rdt
    view :: rdt -> value

instance Replicated a b => Replicated a (Object b) where
    initialize a = Object <$> getEventUuid <*> initialize a
    view (Object _ b) = view b

instance Replicated a b => Replicated a (LwwField b) where
    initialize a = LwwField <$> getEventUuid <*> initialize a
    view (LwwField _ b) = view b

instance Replicated a a where
    initialize = pure
    view = id

instance Replicated Text (RGA Char) where
    initialize =
        fmap RGA . traverse (\a -> (, zero, a) <$> getEventUuid) . Text.unpack
    view (RGA rga) = Text.pack $ map thd3 rga

class ReplicatedAsObject a where
    objectToChunks :: Object a -> [StateChunk]
    objectFromChunks
        :: UUID  -- ^ this object id
        -> Frame'
        -> Either String (Object a)

instance ReplicatedAsPayload a => ReplicatedAsObject (RGA a) where
    objectToChunks (Object oid (RGA rga)) =
        StateChunk{stateType = typeName @Prim.RGA, stateObject = oid, ..}
        : otherChunks
      where
        (otherChunks, ops) =
            for rga $ \(opEvent, opRef, value) -> do
                opPayload <- swap $ toPayload value
                pure Op'{..}
        (stateVersion, stateBody) =
            stateToChunk . Prim.RGA $ Prim.vertexListFromOps ops

    objectFromChunks oid chunks = do
        chunk <- note "no such object in chunk" $
            Map.lookup (typeName @Prim.RGA, oid) chunks
        fmap (Object oid . RGA) . for chunk $ \Op'{..} -> do
            value <- fromPayload opPayload chunks
            pure (opEvent, opRef, value)

-- | TODO Very bad name
class ReplicatedAsPayload a where
    toPayload :: a -> ([Atom], [StateChunk])
    fromPayload :: [Atom] -> Frame' -> Either String a

instance ReplicatedAsPayload Int64 where
    toPayload int = ([AInteger int], [])
    fromPayload atoms _ = case atoms of
        [AInteger int] -> pure int
        _ -> Left "bad payload"

instance ReplicatedAsPayload Char where
    toPayload c = ([AString $ Text.singleton c], [])
    fromPayload atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "bad payload"

instance ReplicatedAsObject a => ReplicatedAsPayload (Object a) where
    toPayload obj@(Object oid _) = ([AUuid oid], objectToChunks obj)
    fromPayload atoms chunks = case atoms of
        [AUuid oid] -> objectFromChunks oid chunks
        _ -> Left "bad payload"

lwwFieldFromOps
    :: ReplicatedAsPayload a
    => UUID -> LwwChunk -> Frame' -> Either String (LwwField a)
lwwFieldFromOps name ops chunks = do
    Op'{..} <- note "no such name in lww chunk" $ Map.lookup name ops
    value <- fromPayload opPayload chunks
    pure $ LwwField opEvent value

{-
Schema:

    type Char
        RON     String
        C++     use wchar_t
        Haskell use Char

    type RgaText
        RON     RGA Char
        Haskell use Text

    struct TestStruct
        fields
            int     SInt64
            text    RgaText
        Haskell
            state_prefix    s_
-}

{- GENERATED -}
intName :: UUID
intName  = fromJust $ UUID.mkName "int"
textName :: UUID
textName = fromJust $ UUID.mkName "text"
data TestStruct = TestStruct{int :: Int64, text :: Text} deriving (Eq, Show)
data TestStructState = TestStructState
    {s_int :: LwwField Int64, s_text :: LwwField (Object (RGA Char))}
    deriving (Eq, Show)
instance Replicated TestStruct TestStructState where
    initialize TestStruct{..} =
        TestStructState <$> initialize int <*> initialize text
    view TestStructState{..} = TestStruct{int = view s_int, text = view s_text}
instance ReplicatedAsObject TestStructState where
    objectToChunks (Object oid TestStructState{..}) =
        StateChunk{stateType = typeName @LwwPerField, stateObject = oid, ..}
        : otherChunks
      where
        LwwField intVersion  intValue  = s_int
        LwwField textVersion textValue = s_text
        (otherChunks, state) = do
            intPayload  <- swap $ toPayload intValue
            textPayload <- swap $ toPayload textValue
            pure . LwwPerField $ Map.fromList
                [ (intName,  Op' intVersion  intName  intPayload )
                , (textName, Op' textVersion textName textPayload)
                ]
        (stateVersion, stateBody) = stateToChunk state
    objectFromChunks oid chunks = do
        chunk <- note "no such object in chunk" $
            Map.lookup (typeName @LwwPerField, oid) chunks
        let ops = Map.fromList [(opRef op, op) | op <- chunk]
        Object oid <$> do
            s_int  <- lwwFieldFromOps intName  ops chunks
            s_text <- lwwFieldFromOps textName ops chunks
            pure TestStructState{..}
{- /GENERATED -}

testStruct0 :: TestStruct
testStruct0 = TestStruct{int = 275, text = "275"}

testStruct1 :: Object TestStructState
testStruct1 = object 159 $ TestStructState
    { s_int = LwwField (event 375) 275
    , s_text = LwwField (event 567) . object 695 $ RGA
        [(event 696, zero, '2'), (event 767, zero, '7'), (event 1015, zero, '5')]
    }
  where
    event t = encodeEvent . fromEpochEvent $ EpochEvent (ls60 t) replica
    object o = Object (UUID (0xb000000000000000 + o) 0x2d83d30067100000)

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

testStruct2 :: ByteStringL
testStruct2 = [i|
    *lww #B/000000002V+r3pl1c4 @`000000008s            !
                               @]5s         :int  =275 ,
                               @]8s         :text >]As ,

    *rga #]As                  @]Fs         :0         !
                               @]At               '2'  ,
                               @]B~               '7'  ,
                               @]Fs               '5'  ,
    .
    |]

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    ts1 <- evalEitherS $ runNetworkSim $ runReplicaSim replica $
        initialize @TestStruct testStruct0
    let tsId = objectId ts1
    testStruct1 === ts1
    let ts2 = testStructSerialize ts1
    BSLC.words testStruct2 === BSLC.words ts2
    ts3 <- evalEitherS $ testStructParse tsId ts2
    ts1 === ts3
    testStruct0 === view ts3

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

testStructParse :: UUID -> ByteStringL -> Either String (Object TestStructState)
testStructParse oid = parseFrame >=> objectFromChunks oid . findObjects

testStructSerialize :: Object TestStructState -> ByteStringL
testStructSerialize =
    serializeFrame . map wrapChunk . objectToChunks
  where
    wrapChunk StateChunk{..} = Value RChunk{..} where
        rchunkHeader = Op{op' = Op'{opRef = zero, opPayload = [], ..}, ..}
        rchunkBody = [Op{..} | op' <- stateBody]
        opType = stateType
        opObject = stateObject
        opEvent = stateVersion

data StateChunk = StateChunk
    { stateType    :: UUID
    , stateObject  :: UUID
    , stateVersion :: UUID
    , stateBody    :: [Op']
    }

findObjects :: Frame -> Frame'
findObjects frame = Map.fromList
    [ ((opType, opObject), map op' rchunkBody)
    | Value RChunk{rchunkHeader, rchunkBody} <- frame
    , let Op{opType, opObject} = rchunkHeader
    ]
