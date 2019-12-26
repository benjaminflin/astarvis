module Draw where

import AStar (AStarResult, AStarState, Tile(..))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader.Trans (ReaderT(..), ask, lift, runReaderT)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List, foldMap, foldl, (!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, Transform, clearRect, fillRect, getCanvasElementById, getContext2D, scale, setCanvasDimensions, setFillStyle, transform, translate)
import Prelude (class Show, Unit, bind, const, discard, flip, negate, pure, unit, when, ($), (*), (+), (-), (/), (<<<), (<>))
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Window.DevicePixelRatio (devicePixelRatio)

data Error
  = CanvasNotFoundError
  | OutOfBoundsError

instance showErr :: Show Error where
  show CanvasNotFoundError = "CanvasNotFoundError"
  show OutOfBoundsError = "OutOfBoundsError"

type DrawParams
  = { iteration :: Int
    , world :: Transform
    , renderPath :: Boolean
    , state :: AStarState
    , result :: AStarResult
    }

type Config
  = { state :: AStarState
    , result :: AStarResult
    , world :: Transform
    , iteration :: Int
    , renderPath :: Boolean
    , ctx :: Context2D
    , dimensions :: Dimensions
    , canvas :: CanvasElement
    }

viewScale :: Number
viewScale = 40.0

getCanvas :: ExceptT Error Effect CanvasElement
getCanvas = do
  canvas <- lift $ getCanvasElementById "astar-vis"
  case canvas of
    Just canvas -> pure $ canvas
    Nothing -> throwError CanvasNotFoundError

getWindowDimensions :: Boolean -> Effect (Dimensions)
getWindowDimensions withPixelRatio = do
  window <- window
  width <- innerWidth window
  height <- innerHeight window
  pixelRatio <- devicePixelRatio window
  let
    r = if withPixelRatio then pixelRatio else 1.0
  pure ({ width: toNumber width * r, height: toNumber height * r })

getExpandedStates :: List (Set Tile) -> Int -> Either Error (Set Tile)
getExpandedStates pastExplored iteration = case pastExplored !! iteration of
  Just s -> Right s
  Nothing -> Left OutOfBoundsError

setupCanvas :: CanvasElement -> Effect (Tuple Context2D Dimensions)
setupCanvas canvas = do
  dimensions <- getWindowDimensions true
  setCanvasDimensions canvas dimensions
  ctx <- getContext2D canvas
  clearRect ctx { x: 0.0, y: 0.0, width: dimensions.width, height: dimensions.height }
  pure $ Tuple ctx dimensions

applyTransformations :: ReaderT Config Effect Unit
applyTransformations = do
  { ctx, dimensions, world } <- ask
  lift
    $ do
        translate ctx { translateX: dimensions.width / 2.0, translateY: dimensions.height / 2.0 }
        scale ctx { scaleX: viewScale, scaleY: -viewScale }
        transform ctx world

inverseView :: Vector2 -> Effect Vector2
inverseView vec = do
  { width, height } <- getWindowDimensions false
  pure $ { x: (vec.x - width / 2.0) / (viewScale / 2.0), y: (vec.y - height / 2.0) / (-viewScale / 2.0) }

inverseViewScale :: Vector2 -> Effect Vector2
inverseViewScale vec = do
  pure $ { x: vec.x / (viewScale / 2.0), y: vec.y / (-viewScale / 2.0) }

type Vector2
  = { x :: Number, y :: Number }

drawExpandedStates :: Set Tile -> ReaderT Config Effect Unit
drawExpandedStates states = do
  { ctx } <- ask
  lift $ setFillStyle ctx "#F3C13A"
  lift $ foldMap (drawTile ctx) states
  where
  drawTile ctx (Tile x y) = fillRect ctx { x: toNumber x, y: toNumber y, width: 1.0, height: 1.0 }

drawEndpoints :: ReaderT Config Effect Unit
drawEndpoints = do
  config@{ ctx, state } <- ask
  let
    (Tile startX startY) = state.startTile
  lift
    $ do
        setFillStyle ctx "#eb4034"
        fillRect ctx { x: toNumber startX + (1.0 - scale) / 2.0, y: toNumber startY + (1.0 - scale) / 2.0, width: scale, height: scale }
        setFillStyle ctx "#26A65B"
        foldMap (drawGoal ctx) state.goalTiles
  where
  scale = 0.7

  drawGoal ctx (Tile x y) = fillRect ctx { x: toNumber x + (1.0 - scale) / 2.0, y: toNumber y + (1.0 - scale) / 2.0, width: scale, height: scale }

drawMap :: ReaderT Config Effect Unit
drawMap = do
  config@{ ctx, state } <- ask
  lift
    $ do
        setFillStyle ctx "#757D75"
        foldMap (\(Tile x y) -> fillRect ctx { x: toNumber x, y: toNumber y, width: 1.0, height: 1.0 }) state.map

drawPath :: ReaderT Config Effect Unit
drawPath = do
  config@{ ctx, result } <- ask
  lift $ setFillStyle ctx "#1F4788"
  result `maybeDo` (lift <<< foldMap (\(Tuple (Tile x y) _) -> fillRect ctx { x: toNumber x, y: toNumber y, width: 1.0, height: 1.0 }))
  where
  maybeDo = flip $ maybe (pure unit)

draw :: ExceptT Error (ReaderT Config Effect) Unit
draw = do
  config <- lift $ ask
  let
    state = config.state
  states <- liftEither $ getExpandedStates state.pastExplored config.iteration
  lift applyTransformations
  lift drawMap
  lift $ drawExpandedStates states
  when config.renderPath $ lift drawPath
  lift drawEndpoints
  where
  liftEither eth = case eth of
    Left e -> throwError e
    Right r -> lift $ ReaderT $ const (pure r)

drawAStar :: DrawParams -> Effect (Either Error Unit)
drawAStar { state, result, iteration, world, renderPath } =
  runExceptT
    $ do
        canvas <- getCanvas
        (Tuple ctx dimensions) <- lift $ setupCanvas canvas
        let
          config = { canvas, ctx, dimensions, state, result, iteration, world, renderPath }
        ExceptT $ runReaderT (runExceptT draw) config
