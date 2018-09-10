{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Error (note)
import           Control.Monad.Writer.Strict (WriterT, lift, runWriterT, tell)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data.Internal (stateFromChunk)
import           RON.Data.VersionVector (VersionVector (..))
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

objectToPayload :: Object Frame' -> WriterT Frame' clock [Atom]
objectToPayload = undefined

-- ReplicatedAsPayload ---------------------------------------------------------

class ReplicatedAsPayload a where
    newPayload :: Clock clock => a -> WriterT Frame' clock [Atom]
    fromPayload :: [Atom] -> Frame' -> Either String a

instance ReplicatedAsPayload Int64 where
    newPayload int = pure [AInteger int]
    fromPayload atoms _ = case atoms of
        [AInteger int] -> pure int
        _ -> Left "Int64: bad payload"

instance ReplicatedAsPayload Text where
    newPayload t = pure [AString t]
    fromPayload atoms _ = case atoms of
        [AString t] -> pure t
        _ -> Left "String: bad payload"

instance ReplicatedAsPayload Char where
    newPayload c = pure [AString $ Text.singleton c]
    fromPayload atoms _ = case atoms of
        [AString s] -> case Text.uncons s of
            Just (c, "") -> pure c
            _ -> Left "too long string to encode a single character"
        _ -> Left "Char: bad payload"

-- LWW -------------------------------------------------------------------------

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

newLwwFrame
    :: Clock clock => [(UUID, I ReplicatedAsPayload)] -> clock (Object Frame')
newLwwFrame fields = collectFrame $ do
    payloads <- for fields $ \(_, I value) -> newPayload value
    e <- lift getEventUuid
    pure $ Object e $ Map.singleton (lwwType, e) $ StateChunk e
        [Op' e name p | ((name, _), p) <- zip fields payloads]

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
            vertexId <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' vertexId zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (rgaType, oid) $ StateChunk version ops
        pure [AUuid oid]

    fromPayload atoms frame = case atoms of
        [AUuid oid] -> do
            StateChunk{..} <-
                note "no such object in chunk" $ Map.lookup (rgaType, oid) frame
            items <- for stateBody $ \Op'{..} -> do
                value <- fromPayload opPayload frame
                pure (opRef, value)
            pure $ RGA [value | (opRef, value) <- items, opRef == zero]
        _ -> Left "RGA: bad payload"

-- ORSetHash -------------------------------------------------------------------

setType :: UUID
setType = fromJust $ UUID.mkName "set"

newtype ORSetHash a = ORSetHash (HashSet a)

instance (Eq a, Hashable a, ReplicatedAsPayload a)
    => ReplicatedAsPayload (ORSetHash a)
    where

    newPayload (ORSetHash items) = do
        ops <- for (toList items) $ \a -> do
            e <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' e zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (setType, oid) $ StateChunk version ops
        pure [AUuid oid]

    fromPayload atoms frame = case atoms of
        [AUuid oid] -> do
            StateChunk{..} <-
                note "no such object in chunk" $ Map.lookup (setType, oid) frame
            items <- for stateBody $ \Op'{..} -> do
                value <- fromPayload opPayload frame
                pure (opRef, value)
            pure $ ORSetHash $ HashSet.fromList
                [value | (opRef, value) <- items, opRef == zero]
        _ -> Left $ "ORSet Hash: bad payload: " ++ show atoms

-- VersionVector ---------------------------------------------------------------

vvType :: UUID
vvType = fromJust $ UUID.mkName "vv"

instance ReplicatedAsPayload VersionVector where
    newPayload (VersionVector vv) = do
        oid <- lift getEventUuid
        let ops = Map.elems vv
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (vvType, oid) $ StateChunk version ops
        pure [AUuid oid]

    fromPayload atoms frame = case atoms of
        [AUuid oid] -> do
            StateChunk{..} <-
                note "no such object in chunk" $ Map.lookup (vvType, oid) frame
            pure $ stateFromChunk stateBody
        _ -> Left "VersionVector: bad payload"

-- Example ---------------------------------------------------------------------

{-
Schema:

    struct_lww Example1
        fields  int1    SInt64
                str2    RGA Char
                str3    String
                set4    ORSet Hash Example2

    struct_lww Example2
        fields vv5 VersionVector
        Haskell deriving Hashable
-}

{- GENERATED -}
int1Name :: UUID
int1Name = fromJust $ UUID.mkName "int1"
str2Name :: UUID
str2Name = fromJust $ UUID.mkName "str2"
str3Name :: UUID
str3Name = fromJust $ UUID.mkName "str3"
set4Name :: UUID
set4Name = fromJust $ UUID.mkName "set4"
vv5Name :: UUID
vv5Name = fromJust $ UUID.mkName "vv5"

data Example1 = Example1
    {int1 :: Int64, str2 :: String, str3 :: Text, set4 :: HashSet Example2}
    deriving (Eq, Show)
newExample1 :: Clock clock => Example1 -> clock (Object Frame')
newExample1 Example1{..} = newLwwFrame
    [ (int1Name, I int1)
    , (set4Name, I $ ORSetHash set4)
    , (str2Name, I $ RGA str2)
    , (str3Name, I str3)
    ]
getExample1 :: UUID -> Frame' -> Either String Example1
getExample1 oid frame = do
    ops <- note "no such object in chunk" $ Map.lookup (lwwType, oid) frame
    int1           <- getLwwField int1Name ops frame
    RGA str2       <- getLwwField str2Name ops frame
    str3           <- getLwwField str3Name ops frame
    ORSetHash set4 <- getLwwField set4Name ops frame
    pure Example1{..}

newtype Example2 = Example2{vv5 :: VersionVector} deriving (Eq, Hashable, Show)
instance ReplicatedAsPayload Example2 where
    newPayload = lift . newExample2 >=> objectToPayload
    fromPayload atoms frame = case atoms of
        [AUuid oid] -> getExample2 oid frame
        _ -> Left "Example2: bad payload"
newExample2 :: Clock clock => Example2 -> clock (Object Frame')
newExample2 Example2{..} = newLwwFrame [(vv5Name, I vv5)]
getExample2 :: UUID -> Frame' -> Either String Example2
getExample2 oid frame = do
    ops <- note "no such object in chunk" $ Map.lookup (lwwType, oid) frame
    vv5 <- getLwwField int1Name ops frame
    pure Example2{..}
{- /GENERATED -}

testStruct0 :: Example1
testStruct0 = Example1{int1 = 275, str2 = "275", str3 = "190", set4 = mempty}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

testStruct2 :: ByteStringL
testStruct2 = [i|
    *lww    #B/00000000B~+r3pl1c4   @`                      !
                                            :int1   =275    ,
                                            :set4   >]2V    ,
                                            :str2   >]At    ,
                                            :str3   '190'   ,

    *rga    #]At                    @]As    :0              !
                                    @]5s            '2'     ,
                                    @]8s            '7'     ,
                                    @]As            '5'     ,

    *set    #]2V                    @`                      !
    .
    |]

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    Object ts1id ts1frame <-
        evalEitherS $ runNetworkSim $ runReplicaSim replica $
        newExample1 testStruct0
    let ts2 = serializeFrame' ts1frame
    BSLC.words testStruct2 === BSLC.words ts2
    ts3frame <- evalEitherS $ parseFrame' ts2
    ts1frame === ts3frame
    testStruct3 <- evalEitherS $ getExample1 ts1id ts3frame
    testStruct0 === testStruct3

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
