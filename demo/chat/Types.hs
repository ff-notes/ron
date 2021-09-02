module Types (Env (..), Message (..), MessageView (..), getMessageView) where

import Control.Concurrent.STM (TChan)
import Control.Lens ((^.))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Time (UTCTime)
import RON.Epoch qualified as Epoch
import RON.Error (MonadE, errorContext, throwError)
import RON.Event (TimeVariety (Epoch), decodeEvent, timeValue, timeVariety)
import RON.Experimental.Data (ReplicatedObject, Repr, decodeObject)
import RON.Experimental.Data.ORSet (ORMap)
import RON.Experimental.Data.ORSet qualified as ORMap
import RON.Store (MonadStore, readObject)
import RON.Types.Experimental (Ref (Ref))

data MessageView = MessageView
  { postTime :: UTCTime
  , content  :: Message
  }
  deriving (Show)

data Message = Message
  { username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject Message where

  type Repr Message = ORMap Text Text

  decodeObject objectId ops =
    errorContext "view @Message" $ do
      ormap :: Repr Message <- decodeObject objectId ops
      username <- ORMap.lookupLwwDecodeThrow "username" ormap
      text     <- ORMap.lookupLwwDecodeThrow "text"     ormap
      pure Message{username, text}

getMessageView ::
  (MonadE m, MonadStore m) => Ref Message -> m (Maybe MessageView)
getMessageView ref@(Ref objectId _) = do
  postTime <- let
    time = decodeEvent objectId ^. #time
    in
    case timeVariety time of
      Epoch -> pure $ Epoch.decode $ timeValue time
      _     -> throwError "objectId in not an epoch event"
  mMsg <- readObject ref
  pure $ mMsg <&> \content -> MessageView{postTime, content}

data Env = Env
  { username             :: Text
  , onMessagePosted      :: TChan Message
  , onMessageListUpdated :: TChan [MessageView]
  }
