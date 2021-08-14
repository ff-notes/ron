module UI (runUI) where

import Prelude hiding (id)

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.State.Strict (evalState, get, modify)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB (RGB (..))
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Tree (Tree (Node))
import Data.Word (Word64)
import Graphics.Gloss (Color, Display (InWindow), Picture, Point, circle,
                       circleSolid, color, line, makeColor, translate, white)
import Graphics.Gloss.Data.Point.Arithmetic qualified as Point
import Graphics.Gloss.Data.Vector (magV, normalizeV, Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort (..), applyViewPortToPicture,
                                     invertViewPort, viewPortInit)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey, EventMotion, EventResize),
                                         Key (MouseButton), KeyState (Down),
                                         MouseButton (LeftButton), playIO)
import RON.Data.GTree qualified as GTree
import RON.Store.Sqlite (fetchUpdates, runStore)
import RON.Store.Sqlite qualified as Store (Handle)
import RON.Types (UUID)
import RON.UUID.Experimental (origin)
import RON.Util.Word (safeCast)
import UnliftIO (MonadUnliftIO, TChan, atomically, liftIO, tryReadTChan,
                 withRunInIO)

import Database (loadTheTree, theTreeRef)

type Size = (Int, Int)

data Bud = Bud{id :: UUID, x :: Float, y :: Float}

data World = World
  { target      :: Maybe Bud
  , tree        :: Tree Bud
  , viewPort    :: ViewPort
  , windowSize  :: Size
  , cursorPos   :: Vector
  }

runUI :: (MonadLogger m, MonadUnliftIO m) => Store.Handle -> m ()
runUI db = do
  theTree <- loadTheTree db
  let worldInit = resetFromRon windowSize theTree
  updates <- fetchUpdates db
  withRunInIO \run -> do
    let onEvent' event world = run $ onEvent db event   world
    let onTick'  _dt   world = run $ onTick  db updates world
    liftIO $ playIO display white 30 worldInit draw' onEvent' onTick'

  where

    draw' = pure . draw
    display = InWindow "RON Garden" windowSize (400, 300)
    windowSize = (800, 500)

dstV (x, y) (x', y') = sqrt (((x - x') ** 2) + ((y - y') ** 2))
sclV (x, y) scl = (x * scl, y * scl)
addV (x, y) (x', y') = (x + x', y + y')

transformMouseCursor :: World -> Vector -> Vector

transformMouseCursor world@World{cursorPos, windowSize} pos = addV pos $ sclV vec len where
  dst = dstV cursorPos pos
  area = 250
  lensing = 3
  vecN = normalizeV vec
  vec = pos Point.- cursorPos
  len
    | dst < area = lensing - 1
    | dst < lensing * area =
      let
        lensingP = (1 -) $ (dst - area) / (lensing * area)
      in
        (lensingP * lensing) - 1
    | otherwise = 0


draw :: World -> Picture
draw w@World{tree, target, viewPort, windowSize=(wx, wy), cursorPos} = applyViewPortToPicture viewPort pic where

  pic = mconcat $ targetPic : walk tree

  tp = transformMouseCursor w
  stranslate x y = let (x', y') = tp (x, y) in translate x' y'

  -- TODO a kind of zigomorphism?
  walk (Node Bud{x, y} subs) =
    [ color (colorFromOrigin id') $ line $ map tp  [(x, y), (x', y')]
    | Node Bud{x = x', y = y', id = id'} _ <- subs
    ]
    ++ concatMap walk subs

  targetPic =
    foldMap
      (\Bud{x, y} -> stranslate x y $ circle targetRadius <> circleSolid 1)
      target

zoom :: Size -> Tree Bud -> ViewPort
zoom (windowWidth, windowHeight) tree =
  viewPortInit
    { viewPortTranslate = Point.negate center
    , viewPortScale     = min scaleX scaleY
    }
  where

    scaleX = fromIntegral windowWidth  / (baseWidth  + 2 * padding)
    scaleY = fromIntegral windowHeight / (baseHeight + 2 * padding)

    -- TODO a single run?
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
          pure world -- TODO optimistic UI: apply immediately
    EventKey{} -> pure world
    EventMotion (invertViewPort vp -> m) ->
      pure world{target = targetNear tree m, cursorPos = m}
    EventResize windowSize -> pure $ reset windowSize tree

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

onTick ::
  (MonadLogger m, MonadUnliftIO m) =>
  Store.Handle -> TChan a -> World -> m World
onTick db updates world@World{windowSize} = do
  mupdate <- atomically $ tryReadTChan updates
  case mupdate of
    Nothing -> pure world
    Just _patch -> do
      -- TODO apply only patch
      theTree <- loadTheTree db
      pure $ world{tree = placeBuds theTree}

placeBuds :: Tree UUID -> Tree Bud
placeBuds =
  (`evalState` {- x may be mutated by placement -} 0)
  . go {- y is increasing recursively -} 0
  where

    go y (Node id subs) = do
      xLeft <- get
      subs' <- intersperseSequence (modify (+ leafDistanceX)) $ map (go y') subs
      xRight <- get
      let x = (xLeft + xRight) / 2
      pure $ Node Bud{id, x, y} subs'

      where
        y' = y + levelHeight

-- | Like 'sequence', but original list is interspersed with additional action
intersperseSequence :: Applicative f => f a -> [f b] -> f [b]
intersperseSequence inter = \case
  []    -> pure []
  a:as  -> (:) <$> a <*> traverse (inter *>) as

resetFromRon :: Size -> Tree UUID -> World
resetFromRon windowSize ronTree = reset windowSize $ placeBuds ronTree

reset :: Size -> Tree Bud -> World
reset windowSize tree =
  World{tree, windowSize, viewPort = zoom windowSize tree, target = Nothing, cursorPos = (-1000,-1000)}

leafDistanceX :: Float
leafDistanceX = 10

levelHeight :: Float
levelHeight = 20

padding :: Float
padding = 10

targetRadius :: Float
targetRadius = 8

colorFromOrigin :: UUID -> Color
colorFromOrigin u = makeColor r g b 1 where
  RGB r g b =
    hsv
      (fromIntegral (safeCast (u ^. origin) `mod` 360 :: Word64) :: Float)
      1
      0.8
