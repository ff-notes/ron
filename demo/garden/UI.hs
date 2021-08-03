module UI (runUI) where

import           Prelude hiding (id)

import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.State.Strict (State, evalState, get, modify)
import           Data.Tree (Tree (Node))
import           Graphics.Gloss (Display (InWindow), Picture, line, white)
import           Graphics.Gloss.Interface.IO.Game (Event, playIO)
import qualified RON.Store.Sqlite as Store (Handle)
import           RON.Types (UUID)
import           UnliftIO (MonadUnliftIO, liftIO)

import           Database (loadTheTree)

data Bud = Bud{id :: UUID, x :: Float, y :: Float}

type World = Tree Bud

runUI :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m ()
runUI db = do
  theTree <- loadTheTree db
  let world = placeBuds theTree
  liftIO $ playIO display white 30 world (pure . draw) onEvent onTick

  where
    display = InWindow "RON Garden" (windowWidth, windowHeight) (400, 300)

windowWidth, windowHeight :: Num n => n
windowWidth  = 500
windowHeight = 500

draw :: World -> Picture
draw = mconcat . walk where
  -- TODO a kind of zigomorphism?
  walk (Node Bud{x, y} subs) =
    [line [(x, y), (x', y')] | Node Bud{x = x', y = y'} _ <- subs]
    ++ concatMap walk subs

onEvent :: Event -> World -> IO World
onEvent _event = pure

onTick :: Float -> World -> IO World
onTick _dt = pure

placeBuds :: Tree UUID -> Tree Bud
placeBuds = (`evalState` 0) . go 0 where

  go :: Float -> Tree UUID -> State Float (Tree Bud)
  go y (Node id subs) = do
    xLeft  <- get
    subs'  <- traverse (go $ y + dy) subs
    xRight <- subtract dx <$> get
    modify (+ dx)
    let x =
          case subs of
            [] -> xLeft
            _  -> (xLeft + xRight) / 2
    pure $ Node Bud{id, x, y} subs'

  dx = 10

  dy = 20
