module Types (Env (..), MessageContent (..), MessageView (..)) where

import           Control.Concurrent.STM (TChan)
import           Control.Lens ((^.))
import           Data.Generics.Labels ()
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           RON.Data.Experimental (Rep, ReplicatedObject, view)
import           RON.Data.ORSet.Experimental (ORMap)
import qualified RON.Data.ORSet.Experimental as ORMap
import qualified RON.Epoch as Epoch
import           RON.Error (errorContext, throwError)
import           RON.Event (TimeVariety (Epoch), decodeEvent, timeValue,
                            timeVariety)

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
  type Rep MessageView = ORMap Text Text

  view objectId ormap =
    errorContext "view @MessageView" $ do
      postTime <- let
        time = decodeEvent objectId ^. #time
        in
        case timeVariety time of
          Epoch -> pure $ Epoch.decode $ timeValue time
          _     -> throwError "objectId in not an epoch event"
      username <- ORMap.lookupDecodeLwwThrow "username" ormap
      text     <- ORMap.lookupDecodeLwwThrow "text"     ormap
      pure MessageView{postTime, content = MessageContent{username, text}}

data Env = Env
  { username             :: Text
  , onMessagePosted      :: TChan MessageContent
  , onMessageListUpdated :: TChan [MessageView]
  }
