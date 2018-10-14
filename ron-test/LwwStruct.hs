{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Internal.Prelude

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (execStateT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString (i)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog (MonadTest, Property, property, (===))
import           Hedgehog.Internal.Property (failWith)

import           RON.Data (ReplicatedAsObject (..), getObject, newObject)
import qualified RON.Data.ORSet as ORSet
import qualified RON.Data.RGA as RGA
import           RON.Event (Naming (ApplicationSpecific), ReplicaId (..))
import           RON.Event.Simulation (runNetworkSim, runReplicaSim)
import           RON.Internal.Word (ls60)
import           RON.Text (parseFrame, serializeFrame)
import           RON.Types (Object (..), Op (..), RawOp (..), StateChunk (..),
                            StateFrame, UUID, WireChunk (Value), WireFrame,
                            WireReducedChunk (..))
import           RON.UUID (zero)

import           LwwStruct.Types (Example1 (..), Example2 (..), assign_int1,
                                  assign_str3, has_opt5, zoom_set4, zoom_str2)

-- Common ----------------------------------------------------------------------

parseStateFrame :: ByteStringL -> Either String StateFrame
parseStateFrame = parseFrame >=> findObjects

parseObject :: UUID -> ByteStringL -> Either String (Object a)
parseObject oid bytes = Object oid <$> parseStateFrame bytes

serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame = serializeFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value WireReducedChunk{..}
      where
        wrcHeader = RawOp{op = Op{opRef = zero, opPayload = [], ..}, ..}
        wrcBody = stateBody
        opEvent = stateVersion

serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object oid frame) = (oid, serializeStateFrame frame)

findObjects :: WireFrame -> Either String StateFrame
findObjects = fmap Map.fromList . traverse loadBody where
    loadBody = \case
        Value WireReducedChunk{..} -> do
            let RawOp{..} = wrcHeader
            let Op{..} = op
            let stateVersion = opEvent
            let stateBody = wrcBody
            pure ((opType, opObject), StateChunk{..})
        _ -> Left "expected reduced chunk"

--------------------------------------------------------------------------------

example0 :: Example1
example0 = Example1
    {int1 = 275, str2 = "275", str3 = "190", set4 = mempty, opt5 = Nothing}

-- | "r3pl1c4"
replica :: ReplicaId
replica = ReplicaId ApplicationSpecific (ls60 0xd83d30067100000)

ex1expect :: ByteStringL
ex1expect = [i|
    *lww    #B/)6+r3pl1c4   @`                  !
                                :int1   =275    ,
                                :opt5           ,
                                :set4   >)1     ,
                                :str2   >)5     ,
                                :str3   '190'   ,

    *rga    #)5             @)4 :0              !
                            @)2         '2'     ,
                            @)3         '7'     ,
                            @)4         '5'     ,

    *set    #)1             @`                  !
    .
    |]

ex4expect :: ByteStringL
ex4expect = [i|
    *lww    #B/)6+r3pl1c4   @`)C                    !
                            @)7     :int1   =166    ,
                            @`      :opt5           ,
                                    :set4   >)1     ,
                                    :str2   >)5     ,
                            @)C     :str3   '206'   ,

            #)E             @`      :0              !
                                    :vv5    >)D     ,

    *rga    #)5             @)B     :0              !
                            @)2     :`)8    '2'     ,
                            @)3     :)9     '7'     ,
                            @)A     :0      '1'     ,
                            @)B             '4'     ,
                            @)4             '5'     ,

    *set    #)1             @)F                     !
                                            >)E     ,

    *vv     #)D             @`                      !
    .
    |]

example4expect :: Example1
example4expect = Example1
    { int1 = 166
    , str2 = "145"
    , str3 = "206"
    , set4 = [Example2{vv5 = mempty}]
    , opt5 = Nothing
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
        runNetworkSim $ runReplicaSim replica $ runExceptT $
        (`execStateT` ex2) $ do
            assign_int1 166
            zoom_str2 $ RGA.edit "145"
            assign_str3 "206"
            zoom_set4 $ ORSet.addNewRef Example2{vv5 = mempty}
            opt5IsSet <- has_opt5
            guard $ not opt5IsSet

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
