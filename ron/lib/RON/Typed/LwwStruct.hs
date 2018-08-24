{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module RON.Typed.LwwStruct
    ( Field (..)
    , fromStateChunk
    , fromStateOps
    , toStateChunk
    , toStateOps
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Data.LWW (lwwType)
import           RON.Typed (Object (..), Replicated, objectFromStateOps,
                            objectToStateOps)
import           RON.Types (Atom (..), Op (..), RChunk (..), UUID)
import qualified RON.UUID as UUID

data Field = forall a. Replicated a => Field Text (Object a)

toStateOps :: Text -> [Field] -> UUID -> Either String [Op]
toStateOps = undefined

toStateChunk :: Text -> [Field] -> UUID -> Either String RChunk
toStateChunk structName fields this = do
    fieldOps <- for fields $ \(Field _ object) -> objectToStateOps object
    pure $ RChunk
        { chunkHeader = Op
            { opType = lwwType
            , opObject = this
            , opEvent = this
            , opLocation = UUID.zero
            , opPayload =
                AString structName
                : do
                    Field name Object{objectId} <- fields
                    [AString name, AUuid objectId]
            }
        , chunkBody = mconcat fieldOps
        }

fromStateOps :: Text -> UUID -> HashMap UUID [Op] -> Either String (Object a)
fromStateOps = undefined

fromStateChunk
    :: Replicated a
    => Text -> Op -> HashMap UUID [Op] -> Either String (Object a)
fromStateChunk fieldName header body = case opPayload of
    []            -> Left "Empty payload"
    _:fieldsAtoms -> do
        fieldId <- findField fieldsAtoms
        objectFromStateOps fieldId (HM.lookupDefault [] fieldId body) body
  where
    Op{opPayload} = header
    findField = \case
        [] -> Left $ "No field " ++ show fieldName ++ " in struct"
        AString name : AUuid uuid : fieldsAtoms
            | name == fieldName -> pure uuid
            | otherwise         -> findField fieldsAtoms
        _ -> Left "Bad field specification"
