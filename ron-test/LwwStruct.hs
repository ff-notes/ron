{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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

instance ReplicatedAsPayload Text where
    newPayload t = pure [AString t]
    fromPayload atoms _ = case atoms of
        [AString t] -> pure t
        _ -> Left "bad payload"

instance ReplicatedAsPayload Char where
    newPayload c = pure [AString $ Text.singleton c]
    fromPayload atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "bad payload"

-- LWW -------------------------------------------------------------------------

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

newLwwFrame :: Clock clock => [(UUID, [Atom])] -> clock (Object Frame')
newLwwFrame fields = do
    e <- getEventUuid
    pure $ Object e $ Map.singleton (lwwType, e) $
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

rgaType :: UUID
rgaType = fromJust $ UUID.mkName "rga"

newtype RGA a = RGA [a]

instance ReplicatedAsPayload a => ReplicatedAsPayload (RGA a) where
    newPayload (RGA items) = do
        ops <- for items $ \a -> do
            e <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' e zero payload
        oid <- lift getEventUuid
        tell $ Map.singleton (rgaType, oid) $ StateChunk oid ops
        pure [AUuid oid]

    fromPayload atoms frame = case atoms of
        [AUuid oid] -> RGA <$> getRgaFromFrame oid frame
        _ -> Left "bad payload"

getRgaFromFrame :: ReplicatedAsPayload a => UUID -> Frame' -> Either String [a]
getRgaFromFrame oid frame = do
    StateChunk{..} <-
        note "no such object in chunk" $ Map.lookup (rgaType, oid) frame
    vertices <- for stateBody $ \Op'{..} -> do
        value <- fromPayload opPayload frame
        pure (opRef, value)
    pure [value | (opRef, value) <- vertices, opRef == zero]

-- Example ---------------------------------------------------------------------

{-
Schema:

    struct Example
        fields
            int1    SInt64
            str2    RGA Char
            str3    String
-}

{- GENERATED -}
int1Name :: UUID
int1Name = fromJust $ UUID.mkName "int1"
str2Name :: UUID
str2Name = fromJust $ UUID.mkName "str2"
str3Name :: UUID
str3Name = fromJust $ UUID.mkName "str3"
data Example = Example{int1 :: Int64, str2 :: String, str3 :: Text}
    deriving (Eq, Show)
newExample :: Clock clock => Example -> clock (Object Frame')
newExample Example{..} = collectFrame $ do
    int1' <- newPayload int1
    str2' <- newPayload (RGA str2)
    str3' <- newPayload str3
    lift $ newLwwFrame [(int1Name, int1'), (str2Name, str2'), (str3Name, str3')]
getTestStruct :: UUID -> Frame' -> Either String Example
getTestStruct oid frame = do
    ops <- note "no such object in chunk" $ Map.lookup (lwwType, oid) frame
    int1     <- getLwwField int1Name ops frame
    RGA str2 <- getLwwField str2Name ops frame
    str3     <- getLwwField str3Name ops frame
    pure Example{..}
{- /GENERATED -}

testStruct0 :: Example
testStruct0 = Example{int1 = 275, str2 = "275", str3 = "190"}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

testStruct2 :: ByteStringL
-- TODO #B/]At+r3pl1c4
testStruct2 = [i|
    *lww    #B/00000000At+r3pl1c4   @`                      !
                                            :int1   =275    ,
                                            :str2   >)s     ,
                                            :str3   '190'   ,

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
        newExample testStruct0
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
