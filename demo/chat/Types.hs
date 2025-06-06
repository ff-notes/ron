module Types (Env (..), Message (..), MessageView (..), getMessageView) where

import Control.Concurrent.STM (TChan)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Time (UTCTime)
import Lens.Micro ((^.))
import RON.Epoch qualified as Epoch
import RON.Error (MonadE, errorContext, throwError)
import RON.Event (
    TimeVariety (Epoch),
    timeValue,
    timeVariety,
    unsafeDecodeEvent,
 )
import RON.Experimental.Data (ReplicatedObject, decodeObject, encodeObject)
import RON.Experimental.Data.ORSet (ORMap)
import RON.Experimental.Data.ORSet qualified as ORSet
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

  encodeObject objectId Message{username, text} = do
    ORSet.add_ repr ("username", username)
    ORSet.add_ repr ("text",     text)
    where
      repr = Ref @(ORMap Text Text) objectId []

  decodeObject objectId ops =
    errorContext "view @Message" $ do
      repr :: ORMap Text Text <- ORSet.decode objectId ops
      username <- ORSet.lookupLwwDecodeThrow "username" repr
      text     <- ORSet.lookupLwwDecodeThrow "text"     repr
      pure Message{username, text}

getMessageView ::
  (MonadE m, MonadStore m) => Ref Message -> m (Maybe MessageView)
getMessageView ref@(Ref objectId _) = do
  postTime <-
    case timeVariety objectTime of
      Epoch -> pure $ Epoch.decode $ timeValue objectTime
      _     -> throwError "objectId in not an epoch event"
  mMsg <- readObject ref
  pure $ mMsg <&> \content -> MessageView{postTime, content}
  where
    objectTime = unsafeDecodeEvent objectId ^. #time

data Env = Env
  { username             :: Text
  , onMessagePosted      :: TChan Message
  , onMessageListUpdated :: TChan [MessageView]
  }
