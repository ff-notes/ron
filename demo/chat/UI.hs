module UI (runUI) where

import           Brick (App (App), BrickEvent (VtyEvent), EventM, Next, attrMap,
                        continue, defaultMain, halt, showFirstCursor, str)
import qualified Brick
import           Graphics.Vty (Event (EvKey), Key (KEsc))
import qualified RON.Store.FS as Store (Handle)

runUI :: Store.Handle -> IO ()
runUI _db = defaultMain app initialState

initialState :: ()
initialState = ()

app :: App () e ()
app =
  App
    { appAttrMap      = const $ attrMap mempty []
    , appChooseCursor = showFirstCursor
    , appDraw         = \() -> [str "Esc -> exit, Enter -> send message"]
    , appHandleEvent
    , appStartEvent   = pure
    }

appHandleEvent :: () -> BrickEvent () e -> EventM () (Next ())
appHandleEvent state = \case
  VtyEvent ve -> case ve of
    EvKey KEsc [] -> do
      -- state' <- liftIO $ sync storage state False
      -- halt state'
      halt state
    _ -> do
      -- editor' <- handleEditorEvent ve editor
      -- continue state{editor = editor'}
      continue state
  -- AppEvent () -> do
  --   state' <- liftIO $ sync storage state True
  --   continue state'
  _ -> continue state
