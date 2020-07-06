module UI (runUI) where

import           Brick (App (App), BrickEvent (VtyEvent), EventM, Next, Widget,
                        attrMap, continue, defaultMain, fg, fill, hBox, halt,
                        modifyDefAttr, showFirstCursor, str, txt, vBox, vLimit)
import qualified Brick
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents,
                                     handleEditorEvent, renderEditor)
import           Control.Concurrent.STM (atomically, newTChanIO, writeTChan)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isSpace, ord)
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Graphics.Vty (Color (ISOColor), Event (EvKey),
                               Key (KEnter, KEsc))
import qualified RON.Store.FS as Store (Handle)

import           Database (loadAllMessages)
import           Options (UIOptions (UIOptions))
import qualified Options
import           Types (Env (Env), MessageContent (MessageContent),
                        MessageView (MessageView))
import qualified Types

runUI :: Store.Handle -> UIOptions -> IO ()
runUI db UIOptions{username} = do
  messages       <- loadAllMessages db -- TODO load asynchronously
  newMessageChan <- newTChanIO
  let env = Env{username, newMessageChan}
  finalState <- defaultMain (app env) initialState{messages}
  print finalState -- TODO save

data State = State{messages :: [MessageView], messageInput :: Editor Text ()}
  deriving (Generic, Show)

initialState :: State
initialState = State{messages = [], messageInput = emptyEditor}

app :: Env -> App State e ()
app env@Env{username} =
  App
    { appAttrMap      = const $ attrMap mempty []
    , appChooseCursor = showFirstCursor
    , appDraw         = appDraw        username
    , appHandleEvent  = appHandleEvent env
    , appStartEvent   = pure
    }

appDraw :: Text -> State -> [Widget ()]
appDraw username State{messages, messageInput} =
  [ vBox
      [ fill ' '
      , vBox $ map renderMessage messages
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

appHandleEvent :: Env -> State -> BrickEvent () e -> EventM () (Next State)
appHandleEvent Env{username, newMessageChan} state =
  \case
    VtyEvent ve -> case ve of
      EvKey KEsc [] -> halt state
      EvKey KEnter []
        | not $ Text.all isSpace text -> do
            let message = MessageContent{username, text}
            -- put in database asynchronously
            liftIO $ atomically $ writeTChan newMessageChan message
            continue state{messageInput = emptyEditor}
        where
          text =
            Text.strip $ Text.intercalate "\n" $ getEditContents messageInput
      _ -> do
        messageInput' <- handleEditorEvent ve messageInput
        continue state{messageInput = messageInput'}
    -- AppEvent () -> do
    --   state' <- liftIO $ sync storage state True
    --   continue state'
    _ -> continue state
  where
    State{messageInput} = state

emptyEditor :: Editor Text ()
emptyEditor = editorText () Nothing ""
