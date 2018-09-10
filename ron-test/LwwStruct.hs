{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Monad.Writer.Strict (lift, tell)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (ReplicatedAsObject, ReplicatedAsPayload,
                           collectFrame, fromPayload, getObject,
                           getObjectStateChunk, newObject, newPayload)
import           RON.Data.LWW (getLwwField, lwwType, newLwwFrame)
import           RON.Data.VersionVector (VersionVector (..))
import           RON.Event (Naming (ApplicationSpecific), ReplicaId (..),
                            getEventUuid)
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import           RON.Text (parseFrame, serializeFrame)
import           RON.Types (Chunk (Value), Frame, Frame', Object (..), Op (..),
                            Op' (..), RChunk (..), StateChunk (..), UUID)
import           RON.UUID (zero)
import qualified RON.UUID as UUID

-- Common ----------------------------------------------------------------------

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

-- RGA -------------------------------------------------------------------------

rgaType :: UUID
rgaType = fromJust $ UUID.mkName "rga"

newtype RGA a = RGA [a]

instance ReplicatedAsPayload a => ReplicatedAsPayload (RGA a)

instance ReplicatedAsPayload a => ReplicatedAsObject (RGA a) where
    newObject (RGA items) = collectFrame $ do
        ops <- for items $ \a -> do
            vertexId <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' vertexId zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (rgaType, oid) $ StateChunk version ops
        pure oid

    getObject oid frame = do
        StateChunk{..} <- getObjectStateChunk rgaType oid frame
        items <- for stateBody $ \Op'{..} -> do
            value <- fromPayload opPayload frame
            pure (opRef, value)
        pure $ RGA [value | (opRef, value) <- items, opRef == zero]

-- ORSetHash -------------------------------------------------------------------

setType :: UUID
setType = fromJust $ UUID.mkName "set"

newtype ORSetHash a = ORSetHash (HashSet a)

instance (Eq a, Hashable a, ReplicatedAsPayload a)
    => ReplicatedAsPayload (ORSetHash a)

instance (Eq a, Hashable a, ReplicatedAsPayload a)
    => ReplicatedAsObject (ORSetHash a)
    where

    newObject (ORSetHash items) = collectFrame $ do
        ops <- for (toList items) $ \a -> do
            e <- lift getEventUuid
            payload <- newPayload a
            pure $ Op' e zero payload
        oid <- lift getEventUuid
        let version = maximumDef oid $ map opEvent ops
        tell $ Map.singleton (setType, oid) $ StateChunk version ops
        pure oid

    getObject oid frame = do
        StateChunk{..} <- getObjectStateChunk setType oid frame
        items <- for stateBody $ \Op'{..} -> do
            value <- fromPayload opPayload frame
            pure (opRef, value)
        pure $ ORSetHash $ HashSet.fromList
            [value | (opRef, value) <- items, opRef == zero]

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
instance ReplicatedAsPayload Example1
instance ReplicatedAsObject Example1 where
    newObject Example1{..} = newLwwFrame
        [ (int1Name, I int1)
        , (set4Name, I $ ORSetHash set4)
        , (str2Name, I $ RGA str2)
        , (str3Name, I str3)
        ]
    getObject oid frame = do
        ops <- getObjectStateChunk lwwType oid frame
        int1           <- getLwwField int1Name ops frame
        RGA str2       <- getLwwField str2Name ops frame
        str3           <- getLwwField str3Name ops frame
        ORSetHash set4 <- getLwwField set4Name ops frame
        pure Example1{..}

newtype Example2 = Example2{vv5 :: VersionVector} deriving (Eq, Hashable, Show)
instance ReplicatedAsPayload Example2
instance ReplicatedAsObject Example2 where
    newObject Example2{..} = newLwwFrame [(vv5Name, I vv5)]
    getObject oid frame = do
        ops <- getObjectStateChunk lwwType oid frame
        vv5 <- getLwwField int1Name ops frame
        pure Example2{..}
{- /GENERATED -}

example0 :: Example1
example0 = Example1{int1 = 275, str2 = "275", str3 = "190", set4 = mempty}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

example2 :: ByteStringL
example2 = [i|
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
    Object ex1id ex1frame <-
        evalEitherS $ runNetworkSim $ runReplicaSim replica $
        newObject example0
    let ex2 = serializeFrame' ex1frame
    BSLC.words example2 === BSLC.words ex2
    ex3frame <- evalEitherS $ parseFrame' ex2
    ex1frame === ex3frame
    example3 <- evalEitherS $ getObject ex1id ex3frame
    example0 === example3

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
