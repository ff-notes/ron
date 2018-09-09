{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Error (note)
import           Control.Monad.Writer.Strict (WriterT, lift, runWriterT, tell)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (typeName)
import           RON.Data.LWW (LwwPerField)
import           RON.Data.RGA (RGA)
import           RON.Event (Clock, Naming (ApplicationSpecific), ReplicaId (..),
                            getEventUuid)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import           RON.Text (parseFrame, serializeFrame)
import           RON.Types (Atom (..), Chunk (Value), Frame, Op (..), Op' (..),
                            RChunk (..), UUID)
import           RON.UUID (zero)
import qualified RON.UUID as UUID

-- Common ----------------------------------------------------------------------

-- | (type, object)
type ObjectKey = (UUID, UUID)

data StateChunk = StateChunk
    { stateVersion :: UUID
    , stateBody    :: [Op']
    }
    deriving (Eq, Show)

type Frame' = Map ObjectKey StateChunk

data Object a = Object {- object id -} UUID {- value -} a
    deriving (Eq, Show)

parseFrame' :: ByteStringL -> Either String Frame'
parseFrame' = parseFrame >=> findObjects

serializeFrame' :: Frame' -> ByteStringL
serializeFrame' = serializeFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value RChunk{..} where
        rchunkHeader = Op{op' = Op'{opRef = zero, opPayload = [], ..}, ..}
        rchunkBody = [Op{..} | op' <- stateBody]
        opEvent = stateVersion

findObjects :: Frame -> Either String Frame'
findObjects = fmap Map.fromList . traverse loadBody where
    loadBody = \case
        Value RChunk{..} -> do
            let Op{..} = rchunkHeader
            let Op'{..} = op'
            let stateVersion = opEvent
            stateBody <- for rchunkBody $ loadOp opType opObject
            pure ((opType, opObject), StateChunk{..})
        _ -> Left "expected reduced chunk"
    loadOp chunkType chunkObject Op{..} = do
        when (opType /= chunkType) $
            Left "reduced op type does not match chunk type"
        when (opObject /= chunkObject) $
            Left "reduced op object id does not match chunk object id"
        pure op'

collectFrame
    :: (Functor m, Semigroup f) => WriterT f m (Object f) -> m (Object f)
collectFrame = fmap (\(Object oid f, f') -> Object oid $ f <> f') . runWriterT

-- ReplicatedAsPayload ---------------------------------------------------------

class ReplicatedAsPayload a where
    newPayload :: Clock clock => a -> WriterT Frame' clock [Atom]
    fromPayload :: [Atom] -> Frame' -> Either String a

instance ReplicatedAsPayload Int64 where
    newPayload int = pure [AInteger int]
    fromPayload atoms _ = case atoms of
        [AInteger int] -> pure int
        _ -> Left "bad payload"

instance ReplicatedAsPayload Char where
    newPayload c = pure [AString $ Text.singleton c]
    fromPayload atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "bad payload"

-- LWW -------------------------------------------------------------------------

newLwwFrame :: Clock clock => [(UUID, [Atom])] -> clock (Object Frame')
newLwwFrame fields = do
    e <- getEventUuid
    pure $ Object e $ Map.singleton (typeName @LwwPerField, e) $
        StateChunk e [Op' e name value | (name, value) <- fields]

getLwwField
    :: ReplicatedAsPayload a => UUID -> StateChunk -> Frame' -> Either String a
getLwwField name StateChunk{..} chunks = do
    let ops = filter ((name ==) . opRef) stateBody
    Op'{..} <- case ops of
        []   -> Left "no such name in lww chunk"
        [op] -> pure op
        _    -> Left "unreduced state"
    fromPayload opPayload chunks

-- RGA -------------------------------------------------------------------------

newRgaPayload
    :: (Foldable f, ReplicatedAsPayload a, Clock clock)
    => f a -> WriterT Frame' clock [Atom]
newRgaPayload vertices = do
    ops <- for (toList vertices) $ \a -> do
        e <- lift getEventUuid
        payload <- newPayload a
        pure $ Op' e zero payload
    oid <- lift getEventUuid
    tell $ Map.singleton (typeName @RGA, oid) $ StateChunk oid ops
    pure [AUuid oid]

getRgaFromPayload
    :: ReplicatedAsPayload a => [Atom] -> Frame' -> Either String [a]
getRgaFromPayload atoms frame = case atoms of
    [AUuid oid] -> getRgaFromFrame oid frame
    _ -> Left "bad payload"

getRgaFromFrame :: ReplicatedAsPayload a => UUID -> Frame' -> Either String [a]
getRgaFromFrame oid frame = do
    StateChunk{..} <-
        note "no such object in chunk" $ Map.lookup (typeName @RGA, oid) frame
    vertices <- for stateBody $ \Op'{..} -> do
        value <- fromPayload opPayload frame
        pure (opRef, value)
    pure [value | (opRef, value) <- vertices, opRef == zero]

-- Example ---------------------------------------------------------------------

{-
Schema:

    type Char
        RON     String
        C++     wchar_t
        Haskell Char

    type RgaText
        RON     RGA Char
        Haskell Text

    struct TestStruct
        fields
            int     SInt64
            text    RgaText
-}

{- GENERATED -}
newtype RgaText = RgaText Text
instance ReplicatedAsPayload RgaText where
    newPayload (RgaText text) = newRgaPayload $ Text.unpack text
    fromPayload atoms = fmap (RgaText . Text.pack) . getRgaFromPayload atoms

intName :: UUID
intName  = fromJust $ UUID.mkName "int"
textName :: UUID
textName = fromJust $ UUID.mkName "text"
data TestStruct = TestStruct{int :: Int64, text :: Text} deriving (Eq, Show)
newTestStruct :: Clock clock => TestStruct -> clock (Object Frame')
newTestStruct TestStruct{..} = collectFrame $ do
    int'  <- newPayload int
    text' <- newPayload $ RgaText text
    lift $ newLwwFrame [(intName, int'), (textName, text')]
getTestStruct :: UUID -> Frame' -> Either String TestStruct
getTestStruct oid frame = do
    ops <-
        note "no such object in chunk" $
        Map.lookup (typeName @LwwPerField, oid) frame
    int          <- getLwwField intName  ops frame
    RgaText text <- getLwwField textName ops frame
    pure TestStruct{..}
{- /GENERATED -}

testStruct0 :: TestStruct
testStruct0 = TestStruct{int = 275, text = "275"}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

testStruct2 :: ByteStringL
-- TODO #B/]At+r3pl1c4
testStruct2 = [i|
    *lww    #B/00000000At+r3pl1c4   @`                      !
                                            :int    =275    ,
                                            :text   >)s     ,

    *rga    #)s                     @`      :0              !
                                    @]2V            '2'     ,
                                    @]5s            '7'     ,
                                    @]8s            '5'     ,
    .
    |]

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    Object ts1id ts1frame <-
        evalEitherS $ runNetworkSim $ runReplicaSim replica $
        newTestStruct testStruct0
    let ts2 = serializeFrame' ts1frame
    BSLC.words testStruct2 === BSLC.words ts2
    ts3frame <- evalEitherS $ parseFrame' ts2
    ts1frame === ts3frame
    testStruct3 <- evalEitherS $ getTestStruct ts1id ts3frame
    testStruct0 === testStruct3

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
