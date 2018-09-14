{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Monad.Except (MonadError, runExceptT)
import           Control.Monad.State.Strict (MonadState, StateT, execStateT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String.Interpolate.IsString (i)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (getObject, newObject)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (AsObjectMap (..))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (AsRga (..))
import           RON.Event (Clock, Naming (ApplicationSpecific), ReplicaId (..))
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import           RON.Schema (Declaration (..), Field (..), RonType (..),
                             StructAnnotations (..), StructLww (..), TAtom (..),
                             char)
import           RON.Schema.TH (mkReplicated)
import           RON.Text (parseFrame, serializeFrame)
import           RON.Types (Chunk (Value), Frame, Frame', Object (..), Op (..),
                            Op' (..), RChunk (..), StateChunk (..), UUID)
import           RON.UUID (zero)
import qualified RON.UUID as UUID

-- Common ----------------------------------------------------------------------

parseFrame' :: ByteStringL -> Either String Frame'
parseFrame' = parseFrame >=> findObjects

parseObject :: UUID -> ByteStringL -> Either String (Object a)
parseObject oid bytes = Object oid <$> parseFrame' bytes

serializeFrame' :: Frame' -> ByteStringL
serializeFrame' = serializeFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value RChunk{..} where
        rchunkHeader = Op{op' = Op'{opRef = zero, opPayload = [], ..}, ..}
        rchunkBody = [Op{..} | op' <- stateBody]
        opEvent = stateVersion

serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object oid frame) = (oid, serializeFrame' frame)

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

-- Schema ----------------------------------------------------------------------

$(let
    tExample1 = StructLww
        { structName = "Example1"
        , structFields = Map.fromList
            [ ("int1", Field (TAtom TAInteger) mempty)
            , ("set4", Field (TORSet (TStructLww tExample2)) mempty)
            , ("str2", Field (TRga char) mempty)
            , ("str3", Field (TAtom TAString) mempty)
            ]
        , structAnnotations =
            mempty{saHaskellDeriving = Set.fromList ["Eq", "Show"]}
        }
    tExample2 = StructLww
        { structName = "Example2"
        , structFields = Map.fromList [("vv5", Field TVersionVector mempty)]
        , structAnnotations = mempty
            { saHaskellDeriving =
                Set.fromList ["Eq", "Generic", "Hashable", "Show"]
            }
        }
    in mkReplicated [DStructLww tExample1, DStructLww tExample2])

-- GENERATED -------------------------------------------------------------------

int1Name :: UUID
int1Name = fromJust $ UUID.mkName "int1"
str2Name :: UUID
str2Name = fromJust $ UUID.mkName "str2"
str3Name :: UUID
str3Name = fromJust $ UUID.mkName "str3"
set4Name :: UUID
set4Name = fromJust $ UUID.mkName "set4"

setInt1
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => Int64 -> m ()
setInt1 = LWW.writeField int1Name . I
modifyStr2
    :: MonadError String m
    => StateT (Object (AsRga String)) m () -> StateT (Object Example1) m ()
modifyStr2 = LWW.modifyField str2Name
setStr3
    :: (Clock m, MonadError String m, MonadState (Object Example1) m)
    => Text -> m ()
setStr3 = LWW.writeField str3Name . I
modifySet4
    :: MonadError String m
    => StateT (Object (AsObjectMap Example2)) m ()
    -> StateT (Object Example1) m ()
modifySet4 = LWW.modifyField set4Name

-- /GENERATED ------------------------------------------------------------------

example0 :: Example1
example0 = Example1{int1 = 275, str2 = "275" :: String, str3 = "190", set4 = mempty}

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
    *lww    #B/]B~+r3pl1c4  @`]Gs                   !
                            @]Fs    :int1   =166    ,
                            @`      :set4   >]2V    ,
                                    :str2   >]At    ,
                            @]Gs    :str3   '206'   ,

            #]It            @`      :0              !
                                    :vv5    >)s     ,

    *rga    #]At            @]As    :0              !
                            @]5s            '2'     ,
                            @]8s            '7'     ,
                            @]As            '5'     ,

    *set    #]2V            @]J~                    !
                                            >]It    ,

    *vv     #]Is            @`                      !
    .
    |]

example4expect :: Example1
example4expect = Example1
    { int1 = 166
    , str2 = "275" :: String
    , str3 = "206"
    , set4 = [Example2{vv5 = mempty}]
    }

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    -- create an object
    let ex1 = runNetworkSim $ runReplicaSim replica $ newObject example0
    let (oid, ex1s) = serializeObject ex1
    prep ex1expect === prep ex1s

    -- parse newly created object
    ex2 <- evalEitherS $ parseObject oid ex1s
    ex1 === ex2

    -- decode newly created object
    example3 <- evalEitherS $ getObject ex2
    example0 === example3

    -- apply operations to the object (frame)
    ex4 <- evalEitherS $
        runNetworkSim $ runReplicaSim replica $
        runExceptT $ (`execStateT` ex2) $ do
            setInt1 166
            modifyStr2 $ pure () -- TODO edit
            setStr3 "206"
            modifySet4 $ ORSet.addNewRef Example2{vv5 = mempty}

    -- decode object after modification
    example4 <- evalEitherS $ getObject ex4
    example4expect === example4

    -- serialize object after modification
    prep ex4expect === prep (snd $ serializeObject ex4)

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a
