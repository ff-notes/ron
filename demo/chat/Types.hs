module Types (Env (..), MessageContent (..), MessageView (..)) where

import Control.Concurrent.STM (TChan)
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Time (UTCTime)
import RON.Data.Experimental (ReplicatedObject, Repr, decodeObject)
import RON.Data.ORSet.Experimental (ORMap)
import RON.Data.ORSet.Experimental qualified as ORMap
import RON.Epoch qualified as Epoch
import RON.Error (errorContext, throwError)
import RON.Event (TimeVariety (Epoch), decodeEvent, timeValue, timeVariety)

data MessageView = MessageView
  { postTime :: UTCTime
  , content  :: MessageContent
  }
  deriving (Show)

data MessageContent = MessageContent
  { username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject MessageView where
  type Repr MessageView = ORMap Text Text
  decodeObject objectId ops =
    errorContext "view @MessageView" $ do
      ormap :: Repr MessageView <- decodeObject objectId ops
      postTime <- let
        time = decodeEvent objectId ^. #time
        in
        case timeVariety time of
          Epoch -> pure $ Epoch.decode $ timeValue time
          _     -> throwError "objectId in not an epoch event"
      username <- ORMap.lookupLwwDecodeThrow "username" ormap
      text     <- ORMap.lookupLwwDecodeThrow "text"     ormap
      pure MessageView{postTime, content = MessageContent{username, text}}

data Env = Env
  { username             :: Text
  , onMessagePosted      :: TChan MessageContent
  , onMessageListUpdated :: TChan [MessageView]
  }
