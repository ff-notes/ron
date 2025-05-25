{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module UI (initUI, runUI) where

import Brick (
    App (App),
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Widget,
    attrMap,
    customMainWithDefaultVty,
    fg,
    fill,
    hBox,
    halt,
    modifyDefAttr,
    showFirstCursor,
    str,
    txt,
    txtWrap,
    vBox,
    vLimit,
    vScrollToEnd,
    viewport,
    viewportScroll,
    zoom,
 )
import Brick qualified
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Types ()
import Brick.Widgets.Border (border)
import Brick.Widgets.Edit (
    Editor,
    editorText,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Control.Concurrent.STM (atomically, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Char (isSpace, ord)
import Data.Generics.Labels ()
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Graphics.Vty (Color (ISOColor), Event (EvKey), Key (KEnter, KEsc))
import Graphics.Vty qualified as Vty
import Lens.Micro.Mtl (use, (.=))
import RON.Store.Sqlite qualified as Store (Handle)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)

import Database (loadAllMessages)
import Fork (forkLinked)
import Types (Env (..), Message (..), MessageView (..), postTime)

data Handle = Handle
    { db :: Store.Handle
    , env :: Env
    , onEvent :: BChan AppEvent
    }

initUI :: (MonadIO m) => Store.Handle -> Env -> m Handle
initUI db env = do
    onEvent <- liftIO $ newBChan 10
    pure Handle{db, env, onEvent}

runUI :: (MonadLogger m, MonadUnliftIO m) => Handle -> m ()
runUI Handle{db, env, onEvent} = do
    userMessages <- loadAllMessages db -- TODO load asynchronously
    liftIO $ forkLinked $ eventWorker env onEvent
    (finalState, _vty) <-
        liftIO $
            customMainWithDefaultVty
                (Just onEvent)
                (app env)
                initialState{userMessages}
    $logDebug $ Text.pack $ "Final state = " <> show finalState -- TODO save

data State = State
    { userMessages :: [MessageView]
    , messageInput :: Editor Text VP
    }
    deriving (Generic, Show)

newtype AppEvent = MessageListUpdated [MessageView]

initialState :: State
initialState = State{userMessages = [], messageInput = emptyEditor}

app :: Env -> App State AppEvent VP
app env@Env{username} =
    App
        { appAttrMap = const $ attrMap Vty.defAttr []
        , appChooseCursor = showFirstCursor
        , appDraw = appDraw username
        , appHandleEvent = appHandleEvent env
        , appStartEvent = appStartEvent
        }

-- | Viewport identifier
data VP = VPMessages | VPNewMessageEditor
    deriving (Eq, Ord, Show)

appStartEvent :: EventM VP State ()
appStartEvent = vScrollToEnd $ viewportScroll VPMessages

appDraw :: Text -> State -> [Widget VP]
appDraw username State{userMessages, messageInput} =
    [ vBox
        [ viewport VPMessages Brick.Vertical $
            vBox $
                map renderMessage $
                    sortOn postTime userMessages
        , border $
            vBox
                [ txt $ username <> ":"
                , vLimit (length $ getEditContents messageInput) $
                    renderEditor (txt . Text.unlines) True messageInput
                ]
        , str "Esc -> exit, Enter -> send message"
        ]
    ]

renderMessage :: MessageView -> Widget n
renderMessage MessageView{postTime, content = Message{username, text}} =
    vBox
        [ vLimit 1 {- workaround for not taking 5 extra lines -} $
            hBox [txtWithContentBasedFg username, fill ' ', str $ show postTime]
        , txtWrap text
        , str " "
        ]

txtWithContentBasedFg :: Text -> Widget n
txtWithContentBasedFg t =
    modifyDefAttr (const $ fg color) $ txt t
  where
    color = colors !! (hashish `mod` length colors)
    hashish = sum $ map ord $ Text.unpack t
    colors = map ISOColor [1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14]

appHandleEvent :: Env -> BrickEvent VP AppEvent -> EventM VP State ()
appHandleEvent Env{username, onMessagePosted} event = do
    messageInput <- use #messageInput
    case event of
        VtyEvent ve -> case ve of
            EvKey KEsc [] -> halt
            EvKey KEnter []
                | not $ Text.all isSpace text -> do
                    let message = Message{username, text}
                    -- put in database asynchronously
                    liftIO $ atomically $ writeTChan onMessagePosted message
                    #messageInput .= emptyEditor
              where
                text =
                    Text.strip $
                        Text.intercalate "\n" $
                            getEditContents messageInput
            _ -> do
                vScrollToEnd $ viewportScroll VPMessages
                zoom #messageInput $ handleEditorEvent event
        AppEvent appEvent ->
            case appEvent of
                MessageListUpdated userMessages -> do
                    vScrollToEnd $ viewportScroll VPMessages
                    #userMessages .= userMessages
        _ -> pure ()

emptyEditor :: Editor Text VP
emptyEditor = editorText VPNewMessageEditor Nothing ""

eventWorker :: Env -> BChan AppEvent -> IO ()
eventWorker Env{onMessageListUpdated} onEvent =
    forever $ do
        messages <- atomically $ readTChan onMessageListUpdated
        writeBChan onEvent $ MessageListUpdated messages
