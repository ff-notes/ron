{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Monad.Except (MonadError, runExceptT)
import           Control.Monad.State.Strict (MonadState, execStateT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (ReplicatedAsObject, ReplicatedAsPayload, getObject,
                           getObjectStateChunk, newObject, objectOpType)
import           RON.Data.LWW (getLwwField, lwwType, newLwwFrame, setLwwField)
import           RON.Data.ORSet (ORSetHash (..))
import           RON.Data.RGA (RgaList (..))
import           RON.Data.VersionVector (VersionVector (..))
import           RON.Event (Clock, Naming (ApplicationSpecific), ReplicaId (..))
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

parseObject
    :: forall a
    . ReplicatedAsObject a => UUID -> ByteStringL -> Either String (Object a)
parseObject oid bytes = Object (objectOpType @a, oid) <$> parseFrame' bytes

serializeFrame' :: Frame' -> ByteStringL
serializeFrame' = serializeFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value RChunk{..} where
        rchunkHeader = Op{op' = Op'{opRef = zero, opPayload = [], ..}, ..}
        rchunkBody = [Op{..} | op' <- stateBody]
        opEvent = stateVersion

serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object (_, oid) frame) = (oid, serializeFrame' frame)

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
    objectOpType = lwwType
    newObject Example1{..} = newLwwFrame
        [ (int1Name, I int1)
        , (set4Name, I $ ORSetHash set4)
        , (str2Name, I $ RgaList str2)
        , (str3Name, I str3)
        ]
    getObject obj = do
        ops <- getObjectStateChunk obj
        int1           <- getLwwField int1Name ops obj
        RgaList str2   <- getLwwField str2Name ops obj
        str3           <- getLwwField str3Name ops obj
        ORSetHash set4 <- getLwwField set4Name ops obj
        pure Example1{..}
setInt1
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => Int64 -> m ()
setInt1 = setLwwField int1Name . I
setStr2
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => String -> m ()
setStr2 = setLwwField str2Name . I . RgaList
setStr3
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => Text -> m ()
setStr3 = setLwwField str3Name . I
setSet4
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => HashSet Example2 -> m ()
setSet4 = setLwwField set4Name . I . ORSetHash

newtype Example2 = Example2{vv5 :: VersionVector} deriving (Eq, Hashable, Show)
instance ReplicatedAsPayload Example2
instance ReplicatedAsObject Example2 where
    objectOpType = lwwType
    newObject Example2{..} = newLwwFrame [(vv5Name, I vv5)]
    getObject obj = do
        ops <- getObjectStateChunk obj
        vv5 <- getLwwField int1Name ops obj
        pure Example2{..}
{- /GENERATED -}

example0 :: Example1
example0 = Example1{int1 = 275, str2 = "275", str3 = "190", set4 = mempty}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

ex1expect :: ByteStringL
ex1expect = [i|
    *lww    #B/]B~+r3pl1c4  @`                      !
                                    :int1   =275    ,
                                    :set4   >]2V    ,
                                    :str2   >]At    ,
                                    :str3   '190'   ,

    *rga    #]At            @]As    :0              !
                            @]5s            '2'     ,
                            @]8s            '7'     ,
                            @]As            '5'     ,

    *set    #]2V            @`                      !
    .
    |]

ex4expect :: ByteStringL
ex4expect = [i|
    *lww    #B/]B~+r3pl1c4  @`]Qs                   !
                            @]Fs    :int1   =166    ,
                            @]Qs    :set4   >]Ws    ,
                            @]Gs    :str2   >]Ns    ,
                            @]Os    :str3   '206'   ,

            #]Vs            @`      :0              !
                                    :vv5    >]R~    ,

    *rga    #]At            @]As    :0              !
                            @]5s            '2'     ,
                            @]8s            '7'     ,
                            @]As            '5'     ,

            #]Ns            @]J~                    !
                            @]Is            '2'     ,
                            @)t             '0'     ,
                            @]J~            '8'     ,

    *set    #]2V            @`                      !

            #]Ws            @]Qt                    !
                                            >]Vs    ,

    *vv     #]R~            @`                      !
    .
    |]

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    let ex1 = runNetworkSim $ runReplicaSim replica $ newObject example0
    let (oid, ex1s) = serializeObject ex1
    BSLC.words ex1expect === BSLC.words ex1s

    ex2 <- evalEitherS $ parseObject oid ex1s
    ex1 === ex2

    example3 <- evalEitherS $ getObject ex2
    example0 === example3

    let ex4 = runNetworkSim $ runReplicaSim replica $
            (`execStateT` ex2) $ runExceptT $ do
                setInt1 166
                setStr2 "208"
                setStr3 "206"
                setSet4 $ HashSet.fromList [Example2{vv5 = mempty}]
    BSLC.words ex4expect === BSLC.words (snd $ serializeObject ex4)

    -- TODO collect garbage

    -- TODO test we can parse this back

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
