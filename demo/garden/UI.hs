module UI (runUI) where

import           Prelude hiding (id)

import           Control.Monad.Logger (MonadLogger)
import           Data.Foldable (fold)
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
draw = fold . walk where
  -- TODO a kind of zigomorphism?
  walk (Node Bud{x, y} subs) =
    Node
      (mconcat [line [(x, y), (x', y')] | Node Bud{x = x', y = y'} _ <- subs])
      (map walk subs)

onEvent :: Event -> World -> IO World
onEvent _event = pure

onTick :: Float -> World -> IO World
onTick _dt = pure

placeBuds :: Tree UUID -> Tree Bud
placeBuds = go 0 0 where

  go x y (Node id subs) =
    case subs of
      [] -> Node Bud{id, x, y} []
      _ ->
        Node
          Bud{id, x, y}
          [go (x + dx * i) (y + dy) sub | sub <- subs | i <- [0..]]

  dx = 10

  dy = 10
