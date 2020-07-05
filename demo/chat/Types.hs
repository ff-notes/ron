module Types where

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
import           RON.Types (Payload)

data Message = Message
  { postTime :: UTCTime
  , username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject Message where
  type Rep Message = ORMap Text Payload

  view objectId ormap =
    errorContext "view @Message" $ do
      postTime <- let
        time = decodeEvent objectId ^. #time
        in
        case timeVariety time of
          Epoch -> pure $ Epoch.decode $ timeValue time
          _     -> throwError "objectId in not an epoch event"
      username <- ORMap.lookupLww' "username" ormap
      text     <- ORMap.lookupLww' "text"     ormap
      pure Message{..}
