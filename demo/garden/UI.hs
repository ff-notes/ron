module UI (runUI) where

import           Prelude hiding (id)

import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.State.Strict (evalState, get, modify)
import           Data.Foldable (toList)
import           Data.List (sortOn)
import           Data.Maybe (listToMaybe)
import           Data.Tree (Tree (Node))
import           Graphics.Gloss (Display (InWindow), Picture, circle, color,
                                 line, red, translate, white)
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point
import           Graphics.Gloss.Data.Vector (magV)
import           Graphics.Gloss.Data.ViewPort (ViewPort (..),
                                               applyViewPortToPicture,
                                               invertViewPort, viewPortInit)
import           Graphics.Gloss.Interface.IO.Game (Event (EventKey, EventMotion, EventResize),
                                                   playIO)
import qualified RON.Store.Sqlite as Store (Handle)
import           RON.Types (UUID)
import           UnliftIO (MonadUnliftIO, liftIO)

import           Database (loadTheTree)

data Bud = Bud{id :: UUID, x :: Float, y :: Float}

data World = World{tree :: Tree Bud, target :: Maybe Bud, viewPort :: ViewPort}

runUI :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m ()
runUI db = do
  theTree <- loadTheTree db
  let
    tree  = placeBuds theTree
    world = World{tree, target = Nothing, viewPort = zoom tree}
  liftIO $ playIO display white 30 world (pure . draw) onEvent onTick

  where
    display = InWindow "RON Garden" (windowWidth, windowHeight) (400, 300)

windowWidth, windowHeight :: Num n => n
windowWidth  = 500
windowHeight = 500

draw :: World -> Picture
draw World{tree, target, viewPort} = applyViewPortToPicture viewPort pic where

  pic = mconcat $ targetPic : walk tree

  -- TODO a kind of zigomorphism?
  walk (Node Bud{x, y} subs) =
    [line [(x, y), (x', y')] | Node Bud{x = x', y = y'} _ <- subs]
    ++ concatMap walk subs

  targetPic =
    foldMap
      (\Bud{x, y} -> translate x y $ color red $ circle targetRadius)
      target

zoom :: Tree Bud -> ViewPort
zoom tree =
  viewPortInit
    { viewPortTranslate = Point.negate center
    , viewPortScale     = min scaleX scaleY
    }
  where

    scaleX = windowWidth  / (baseWidth  + 2 * padding)
    scaleY = windowHeight / (baseHeight + 2 * padding)

    left    = minimum $ x <$> tree
    right   = maximum $ x <$> tree
    top     = maximum $ y <$> tree
    bottom  = minimum $ y <$> tree

    baseWidth  = right - left
    baseHeight = top - bottom

    center = ((left + right) / 2, (top + bottom) / 2)

onEvent :: Event -> World -> IO World
onEvent event world@World{viewPort = vp, tree} =
  pure
  case event of
    EventKey{} -> world
    EventMotion (invertViewPort vp -> m) -> world{target}
      where
        target =
          fmap snd $
          listToMaybe $
          sortOn
            fst
            [ (d, bud)
            | bud@Bud{x, y} <- toList tree
            , let d = magV $ (x, y) Point.- m
            , d < targetRadius
            ]
    EventResize{} -> world{target = Nothing}

onTick :: Float -> World -> IO World
onTick _dt = pure

placeBuds :: Tree UUID -> Tree Bud
placeBuds = (`evalState` 0) . go 0 where

  go y (Node id subs) = do
    xLeft   <- get
    subs'   <- traverse (go $ y + levelHeight) subs
    xRight  <- subtract leafDistanceX <$> get
    modify (+ leafDistanceX)
    let x =
          case subs of
            []  -> xLeft
            _   -> (xLeft + xRight) / 2
    pure $ Node Bud{id, x, y} subs'

leafDistanceX :: Float
leafDistanceX = 10

levelHeight :: Float
levelHeight = 20

padding :: Float
padding = 10

targetRadius :: Float
targetRadius = 9
