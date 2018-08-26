{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Typed.LwwStruct
    ( Field (..)
    , fromStateChunk
    , fromStateOps
    , toStateChunk
    , toStateOps
    ) where

import           RON.Internal.Prelude

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Typed (Object (Object), Replicated, objectFromStateOps,
                            objectId, objectToStateOps)
import           RON.Types (Atom (AString, AUuid), Op (Op), RChunk (RChunk),
                            ROp (ROp), UUID, chunkBody, chunkHeader, opObject,
                            opR, opType, ropEvent, ropLocation, ropPayload)
import qualified RON.UUID as UUID

data Field = forall a. Replicated a => Field Text (Object a)

toStateOps :: Text -> [Field] -> UUID -> Either String [Op]
toStateOps = undefined

toStateChunk :: Text -> [Field] -> UUID -> Either String RChunk
toStateChunk structName fields this = do
    fieldOps <- for fields $ \(Field _ object) -> objectToStateOps object
    pure $ RChunk
        { chunkHeader = Op
            { opType   = fromJust $ UUID.mkName "lww"
            , opObject = this
            , opR      = ROp
                { ropEvent    = this
                , ropLocation = UUID.zero
                , ropPayload  =
                    AString structName
                    : do
                        Field name Object{objectId} <- fields
                        [AString name, AUuid objectId]
                }
            }
        , chunkBody = mconcat fieldOps
        }

fromStateOps :: Text -> UUID -> HashMap UUID [Op] -> Either String (Object a)
fromStateOps = undefined

fromStateChunk
    :: Replicated a
    => Text -> Op -> HashMap UUID [Op] -> Either String (Object a)
fromStateChunk fieldName header body = case ropPayload of
    []            -> Left "Empty payload"
    _:fieldsAtoms -> do
        fieldId <- findField fieldsAtoms
        objectFromStateOps fieldId (HM.lookupDefault [] fieldId body) body
  where
    Op{opR = ROp{ropPayload}} = header
    findField = \case
        [] -> Left $ "No field " ++ show fieldName ++ " in struct"
        AString name : AUuid uuid : fieldsAtoms
            | name == fieldName -> pure uuid
            | otherwise         -> findField fieldsAtoms
        _ -> Left "Bad field specification"
