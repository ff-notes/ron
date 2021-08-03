module UI (runUI) where

import           Graphics.Gloss (Display (InWindow), Picture, white)
import           Graphics.Gloss.Interface.IO.Game (Event, playIO)
import           UnliftIO (MonadIO, liftIO)

-- import           RON.Store.Sqlite (runStore)
import qualified RON.Store.Sqlite as Store (Handle)

data World = World

runUI :: MonadIO m => Store.Handle -> m ()
runUI _db = liftIO $ playIO display white 30 World draw onEvent onTick where
  display = InWindow "RON Garden" (windowWidth, windowHeight) (400, 300)
  windowWidth  = 500
  windowHeight = 500

draw :: World -> IO Picture
draw _world = pure mempty

onEvent :: Event -> World -> IO World
onEvent _event = pure

onTick :: Float -> World -> IO World
onTick _dt = pure
