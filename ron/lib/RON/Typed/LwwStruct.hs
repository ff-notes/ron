{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module RON.Typed.LwwStruct
    ( Field (..)
    , toStateChunk
    , toStateOps
    ) where

import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Data.LWW (lwwType)
import           RON.Typed (Object (..), Replicated, objectToStateOps)
import           RON.Types (Atom (..), Op (..), ReducedChunk (..), UUID)
import qualified RON.UUID as UUID

data Field = forall a. Replicated a => Field Text (Object a)

toStateOps :: Text -> [Field] -> UUID -> Either String [Op]
toStateOps = undefined
    --     bodyInt  <- objectToStateOps ts_int
    --     bodyText <- objectToStateOps ts_text
    --   where
    --     Object{objectId = int_id}  = ts_int
    --     Object{objectId = text_id} = ts_text
    --     opPayload =
    --         [ AString "TestStruct"
    --         , AString "int",  AUuid int_id
    --         , AString "text", AUuid text_id
    --         ]

toStateChunk :: Text -> [Field] -> UUID -> Either String ReducedChunk
toStateChunk structName fields this = do
    fieldOps <- for fields $ \(Field _ object) -> objectToStateOps object
    pure $ ReducedChunk
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
