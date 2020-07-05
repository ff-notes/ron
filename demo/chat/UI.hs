module UI (runUI) where

import           Brick (App (App), BrickEvent (VtyEvent), EventM, Next, Widget,
                        attrMap, continue, defaultMain, halt, showFirstCursor,
                        txt, vBox, vLimit)
import qualified Brick
import           Brick.Widgets.Border (border, hBorder)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents,
                                     handleEditorEvent, renderEditor)
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty (Event (EvKey), Key (KEsc))
import qualified RON.Store.FS as Store (Handle)

import           Options (UIOptions (UIOptions))
import qualified Options

runUI :: Store.Handle -> UIOptions -> IO ()
runUI _db UIOptions{username} = void $ defaultMain (app username) initialState

newtype State = State{messageInput :: Editor Text ()}

initialState :: State
initialState = State{messageInput = editorText () Nothing ""}

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
appDraw username State{messageInput} =
  [ vBox
      [ hBorder
      , txt $ username <> ":"
      , border $ vLimit (length $ getEditContents messageInput) $ renderEditor (txt . Text.unlines) True messageInput
      , txt "Esc -> exit, Enter -> send message"
      ]
  ]

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
