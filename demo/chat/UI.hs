module UI (initUI, runUI) where

import           Brick (App (App), BrickEvent (AppEvent, VtyEvent), EventM,
                        Next, Widget, attrMap, continue, customMain, fg, fill,
                        hBox, halt, modifyDefAttr, showFirstCursor, str, txt,
                        vBox, vLimit)
import qualified Brick
import           Brick.BChan (BChan, newBChan, writeBChan)
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents,
                                     handleEditorEvent, renderEditor)
import           Control.Concurrent.STM (atomically, readTChan, writeTChan)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isSpace, ord)
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Graphics.Vty (Color (ISOColor), Event (EvKey),
                               Key (KEnter, KEsc), mkVty)
import qualified Graphics.Vty as Vty
import qualified RON.Store.Sqlite as Store (Handle)

import           Database (loadAllMessages)
import           Fork (forkLinked)
import           Types (Env (Env), MessageContent (MessageContent),
                        MessageView (MessageView), postTime)
import qualified Types

data Handle = Handle
  { db      :: Store.Handle
  , env     :: Env
  , onEvent :: BChan AppEvent
  }

initUI :: Store.Handle -> Env -> IO Handle
initUI db env = do
  onEvent <- newBChan 10
  pure Handle{db, env, onEvent}

runUI :: Handle -> IO ()
runUI Handle{db, env, onEvent} =
  do
    userMessages <- loadAllMessages db -- TODO load asynchronously
    forkLinked $ eventWorker env onEvent
    initialVty <- buildVty
    finalState <-
      customMain
        initialVty
        buildVty
        (Just onEvent)
        (app env)
        initialState{userMessages}
    print finalState -- TODO save
  where
    buildVty = mkVty Vty.defaultConfig

data State = State
  { userMessages   :: [MessageView]
  , logMessages :: [MessageView]
  , messageInput   :: Editor Text ()
  }
  deriving (Generic, Show)

newtype AppEvent = MessageListUpdated [MessageView]

initialState :: State
initialState =
  State{userMessages = [], logMessages = [], messageInput = emptyEditor}

app :: Env -> App State AppEvent ()
app env@Env{username} =
  App
    { appAttrMap      = const $ attrMap mempty []
    , appChooseCursor = showFirstCursor
    , appDraw         = appDraw        username
    , appHandleEvent  = appHandleEvent env
    , appStartEvent   = pure
    }

appDraw :: Text -> State -> [Widget ()]
appDraw username State{userMessages, logMessages, messageInput} =
  [ vBox
      [ fill ' '
      , vBox $
        map renderMessage $ sortOn postTime $ userMessages <> logMessages
      , border $
        vBox
          [ txt $ username <> ":"
          , vLimit (length $ getEditContents messageInput) $
            renderEditor (txt . Text.unlines) True messageInput
          ]
      , str "Esc -> exit, Enter -> send message"
      ]
  ]

renderMessage :: MessageView -> Widget ()
renderMessage MessageView{postTime, content = MessageContent{username, text}} =
  vBox
    [ vLimit 1 {- workaround for not taking 5 extra lines -} $
      hBox [txtWithContentBasedFg username, fill ' ', str $ show postTime]
    , txt text
    , str " "
    ]

txtWithContentBasedFg :: Text -> Widget n
txtWithContentBasedFg t =
  modifyDefAttr (const $ fg color) $ txt t
  where
    color = colors !! (hashish `mod` length colors)
    hashish = fromIntegral $ sum $ map ord $ Text.unpack t
    colors = map ISOColor [1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14]

appHandleEvent ::
  Env -> State -> BrickEvent () AppEvent -> EventM () (Next State)
appHandleEvent Env{username, onMessagePosted} state =
  \case
    VtyEvent ve -> case ve of
      EvKey KEsc [] -> halt state
      EvKey KEnter []
        | not $ Text.all isSpace text -> do
            let message = MessageContent{username, text}
            -- put in database asynchronously
            liftIO $ atomically $ writeTChan onMessagePosted message
            continue state{messageInput = emptyEditor}
        where
          text =
            Text.strip $ Text.intercalate "\n" $ getEditContents messageInput
      _ -> do
        messageInput' <- handleEditorEvent ve messageInput
        continue state{messageInput = messageInput'}
    AppEvent appEvent ->
      case appEvent of
        MessageListUpdated userMessages -> continue state{userMessages}
    _ -> continue state
  where
    State{messageInput} = state

emptyEditor :: Editor Text ()
emptyEditor = editorText () Nothing ""

eventWorker :: Env -> BChan AppEvent -> IO ()
eventWorker Env{onMessageListUpdated} onEvent =
  forever $ do
    messages <- atomically $ readTChan onMessageListUpdated
    writeBChan onEvent $ MessageListUpdated messages
