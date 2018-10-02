{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Data.LWW
    ( LwwPerField (..)
    , getField
    , lwwType
    , newFrame
    , withField
    , writeField
    ) where

import           RON.Internal.Prelude

import           Control.Error (fmapL)
import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (MonadState, StateT, execStateT,
                                             get, put)
import           Control.Monad.Writer.Strict (lift, runWriterT, tell)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Event (Clock, advanceToUuid, getEventUuid)
import           RON.Types (Atom (AUuid), Frame', Object (..), Op (..),
                            StateChunk (..), UUID)
import qualified RON.UUID as UUID

lww :: Op -> Op -> Op
lww = maxOn opEvent

-- | Key is 'opRef', value is the original op
newtype LwwPerField = LwwPerField (Map UUID Op)
    deriving (Eq, Monoid, Show)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    reducibleOpType = lwwType

    stateFromChunk ops =
        LwwPerField $ Map.fromListWith lww [(opRef op, op) | op <- ops]

    stateToChunk (LwwPerField fields) = mkStateChunk $ Map.elems fields

lwwType :: UUID
lwwType = fromJust $ UUID.mkName "lww"

newFrame :: Clock clock => [(UUID, I Replicated)] -> clock (Object a)
newFrame fields = collectFrame $ do
    payloads <- for fields $ \(_, I value) -> newRon value
    e <- lift getEventUuid
    tell $ Map.singleton (lwwType, e) $ StateChunk e
        [Op e name p | ((name, _), p) <- zip fields payloads]
    pure e

getField :: Replicated a => UUID -> StateChunk -> Frame' -> Either String a
getField field StateChunk{..} frame =
    fmapL (("LWW.getField " <> show field <> ":\n") <>) $ do
        let ops = filter ((field ==) . opRef) stateBody
        Op{..} <- case ops of
            []   -> Left $ unwords ["no field", show field, "in lww chunk"]
            [op] -> pure op
            _    -> Left "unreduced state"
        fromRon opPayload frame

writeField
    :: forall a m
    .   ( ReplicatedAsObject a
        , Clock m, MonadError String m, MonadState (Object a) m
        )
    => UUID -> I Replicated -> m ()
writeField field (I value) = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    advanceToUuid stateVersion
    let chunk = filter ((field /=) . opRef) stateBody
    e <- getEventUuid
    (p, frame') <- runWriterT $ newRon value
    let newOp = Op e field p
    let chunk' = sortOn opRef $ newOp : chunk
    let state' = StateChunk e chunk'
    put Object
        { objectFrame =
            Map.insert (objectOpType @a, objectId) state' objectFrame <> frame'
        , ..
        }

withField
    :: (ReplicatedAsObject outer, MonadError String m)
    => UUID -> StateT (Object inner) m () -> StateT (Object outer) m ()
withField field innerModifier = do
    obj@Object{..} <- get
    StateChunk{..} <- either throwError pure $ getObjectStateChunk obj
    let ops = filter ((field ==) . opRef) stateBody
    Op{..} <- case ops of
        []   -> throwError $ unwords ["no field", show field, "in lww chunk"]
        [op] -> pure op
        _    -> throwError "unreduced state"
    innerObjectId <- case opPayload of
        [AUuid oid] -> pure oid
        _           -> throwError "bad payload"
    let innerObject = Object innerObjectId objectFrame
    Object{objectFrame = objectFrame'} <-
        lift $ execStateT innerModifier innerObject
    put Object{objectFrame = objectFrame', ..}
