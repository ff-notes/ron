module UI (runUI) where

import Prelude hiding (id)

import Control.Monad.Logger (MonadLogger)
import Control.Monad.State.Strict (evalState, get, modify)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Tree (Tree (Node))
import Graphics.Gloss (Display (InWindow), Picture, Point, circle, circleSolid,
                       color, line, red, translate, white)
import Graphics.Gloss.Data.Point.Arithmetic qualified as Point
import Graphics.Gloss.Data.Vector (magV)
import Graphics.Gloss.Data.ViewPort (ViewPort (..), applyViewPortToPicture,
                                     invertViewPort, viewPortInit)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey, EventMotion, EventResize),
                                         Key (MouseButton), KeyState (Down),
                                         MouseButton (LeftButton), playIO)
import RON.Data.GTree qualified as GTree
import RON.Store.Sqlite (runStore)
import RON.Store.Sqlite qualified as Store (Handle)
import RON.Types (UUID)
import UnliftIO (MonadUnliftIO, liftIO, withRunInIO)

import Database (loadTheTree, theTreeRef)

data Bud = Bud{id :: UUID, x :: Float, y :: Float}

data World = World{tree :: Tree Bud, target :: Maybe Bud, viewPort :: ViewPort}

runUI :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m ()
runUI db = do
  theTree <- loadTheTree db
  let tree = placeBuds theTree
  let worldInit = World{tree, target = Nothing, viewPort = zoom tree}
  withRunInIO \run -> do
    let onEvent' event world = run $ onEvent db event world
    liftIO $ playIO display white 30 worldInit (pure . draw) onEvent' onTick
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
      ( \Bud{x, y} ->
          translate x y $ color red $ circle targetRadius <> circleSolid 1
          -- TODO <> text (show id)
      )
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

onEvent ::
  (MonadLogger m, MonadUnliftIO m) => Store.Handle -> Event -> World -> m World
onEvent db event world@World{viewPort = vp, tree, target} =
  case event of
    EventKey (MouseButton LeftButton) Down _ _
      | Just Bud{id = parent} <- target -> do
          runStore db $ GTree.insert theTreeRef parent
          pure world
    EventKey{} -> pure world
    EventMotion (invertViewPort vp -> m) ->
      pure world{target = targetNear tree m}
    EventResize{} -> pure world{target = Nothing}

targetNear :: Tree Bud -> Point -> Maybe Bud
targetNear tree m =
  fmap snd $
  listToMaybe $
  sortOn
    fst
    [ (d, bud)
    | bud@Bud{x, y} <- toList tree
    , let d = magV $ (x, y) Point.- m
    , d < targetRadius
    ]

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
targetRadius = 8
