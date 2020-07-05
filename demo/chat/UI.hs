module UI (runUI) where

import           Brick (App (App), BrickEvent (VtyEvent), EventM, Next, Widget,
                        attrMap, continue, defaultMain, fill, halt,
                        showFirstCursor, str, txt, vBox, vLimit)
import qualified Brick
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents,
                                     handleEditorEvent, renderEditor)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty (Event (EvKey), Key (KEsc))
import qualified RON.Store.FS as Store (Handle)

import           Database (loadAllMessages)
import           Options (UIOptions (UIOptions))
import qualified Options
import           Types (Message (Message))
import qualified Types

runUI :: Store.Handle -> UIOptions -> IO ()
runUI db UIOptions{username} = do
  messages   <- loadAllMessages db -- TODO load asynchronously
  finalState <- defaultMain (app username) initialState{messages}
  print finalState -- TODO save

data State = State{messages :: [Message], messageInput :: Editor Text ()}
  deriving (Show)

initialState :: State
initialState = State{messages = [], messageInput = editorText () Nothing ""}

app :: Text -> App State e ()
app username =
  App
    { appAttrMap      = const $ attrMap mempty []
    , appChooseCursor = showFirstCursor
    , appDraw         = appDraw username
    , appHandleEvent
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
      , txt "Esc -> exit, Enter -> send message"
      ]
  ]

renderMessage :: Message -> Widget ()
renderMessage Message{username, postTime, text} =
  vBox [txt $ username <> ":", str $ show postTime, txt text, str " "]

appHandleEvent :: State -> BrickEvent () e -> EventM () (Next State)
appHandleEvent state@State{messageInput} = \case
  VtyEvent ve -> case ve of
    EvKey KEsc [] -> do
      -- state' <- liftIO $ sync storage state False
      -- halt state'
      halt state
    _ -> do
      messageInput' <- handleEditorEvent ve messageInput
      continue state{messageInput = messageInput'}
  -- AppEvent () -> do
  --   state' <- liftIO $ sync storage state True
  --   continue state'
  _ -> continue state
