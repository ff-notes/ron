module UI (runUI) where

import           Brick (App (App), BrickEvent (VtyEvent), EventM, Next, Widget,
                        attrMap, continue, defaultMain, fg, fill, hBox, halt,
                        modifyDefAttr, showFirstCursor, str, txt, vBox, vLimit)
import qualified Brick
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents,
                                     handleEditorEvent, renderEditor)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isSpace, ord)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (getCurrentTime)
import           GHC.Generics (Generic)
import           Graphics.Vty (Color (ISOColor), Event (EvKey),
                               Key (KEnter, KEsc))
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
  deriving (Generic, Show)

initialState :: State
initialState = State{messages = [], messageInput = emptyEditor}

app :: Text -> App State e ()
app username =
  App
    { appAttrMap      = const $ attrMap mempty []
    , appChooseCursor = showFirstCursor
    , appDraw         = appDraw        username
    , appHandleEvent  = appHandleEvent username
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
  vBox
    [ vLimit 1 $ hBox [txt' username, fill ' ', str $ show postTime]
    , txt text
    , str " "
    ]

txt' :: Text -> Widget n
txt' t =
  modifyDefAttr (const $ fg color) $ txt t
  where
    color = colors !! (hashish `mod` length colors)
    hashish = fromIntegral $ sum $ map ord $ Text.unpack t
    colors = map ISOColor [1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14]

appHandleEvent :: Text -> State -> BrickEvent () e -> EventM () (Next State)
appHandleEvent username state@State{messageInput, messages} = \case
  VtyEvent ve -> case ve of
    EvKey KEsc [] -> halt state
    EvKey KEnter []
      | not $ Text.all isSpace text -> do
          postTime <- liftIO getCurrentTime
          let message = Message{..}
          -- TODO put in database
          continue $
            state
              { messageInput = emptyEditor
              , messages     = messages <> [message]
              }
      where
        text = Text.strip $ Text.intercalate "\n" $ getEditContents messageInput
    _ -> do
      messageInput' <- handleEditorEvent ve messageInput
      continue state{messageInput = messageInput'}
  -- AppEvent () -> do
  --   state' <- liftIO $ sync storage state True
  --   continue state'
  _ -> continue state

emptyEditor :: Editor Text ()
emptyEditor = editorText () Nothing ""
