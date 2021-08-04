module UI (runUI) where

import           Prelude hiding (id)

import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.State.Strict (evalState, get, modify)
import           Data.Function (on)
import           Data.List (maximumBy)
import           Data.Tree (Tree (Node))
import           Graphics.Gloss (Display (InWindow), Picture, circle, color,
                                 line, red, scale, translate, white)
import           Graphics.Gloss.Interface.IO.Game (Event, playIO)
import qualified RON.Store.Sqlite as Store (Handle)
import           RON.Types (UUID)
import           UnliftIO (MonadUnliftIO, liftIO)

import           Database (loadTheTree)

data Bud = Bud{id :: UUID, x :: Float, y :: Float}

data World = World{tree :: Tree Bud, target :: Maybe Bud}

runUI :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m ()
runUI db = do
  theTree <- loadTheTree db
  let world = World{tree = placeBuds theTree, target = Nothing}
  liftIO $ playIO display white 30 world (pure . draw) onEvent onTick

  where
    display = InWindow "RON Garden" (windowWidth, windowHeight) (400, 300)

windowWidth, windowHeight :: Num n => n
windowWidth  = 500
windowHeight = 500

draw :: World -> Picture
draw World{tree, target} = scaled where

  scaled =
    scale
      (windowWidth  / (baseWidth  + 2 * padding))
      (windowHeight / (baseHeight + 2 * padding))
      centered

  centered = translate (-centerX) (-centerY) basePicture

  basePicture = mconcat $ targetPic : walk tree

  left    = minimum (x <$> tree)
  right   = maximum (x <$> tree)
  top     = maximum (y <$> tree)
  bottom  = minimum (y <$> tree)

  baseWidth  = right - left
  baseHeight = top - bottom

  centerX = (left + right) / 2
  centerY = (top + bottom) / 2

  -- TODO a kind of zigomorphism?
  walk (Node Bud{x, y} subs) =
    [line [(x, y), (x', y')] | Node Bud{x = x', y = y'} _ <- subs]
    ++ concatMap walk subs

  targetPic :: Picture
  targetPic =
    foldMap
      (\Bud{x, y} -> translate x y $ color red $ circle targetRadius)
      target

onEvent :: Event -> World -> IO World
onEvent _event world@World{tree} = pure world{target = Just $ maximumOn y tree}

onTick :: Float -> World -> IO World
onTick _dt = pure

placeBuds :: Tree UUID -> Tree Bud
placeBuds = (`evalState` 0) . go 0 where

  go y (Node id subs) = do
    xLeft  <- get
    subs'  <- traverse (go $ y + levelHeight) subs
    xRight <- subtract leafDistanceX <$> get
    modify (+ leafDistanceX)
    let x =
          case subs of
            [] -> xLeft
            _  -> (xLeft + xRight) / 2
    pure $ Node Bud{id, x, y} subs'

leafDistanceX :: Float
leafDistanceX = 10

levelHeight :: Float
levelHeight = 20

padding :: Float
padding = 10

targetRadius :: Float
targetRadius = 9

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)
