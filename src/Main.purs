module Main where

import Prelude
import AStar (AStarMap, AStarResult, AStarState, Tile(..), astar, manhattan)
import Control.Coroutine (Consumer, await, pullFrom, runProcess)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, modify, runState, runStateT)
import Control.Monad.State.Trans (lift, StateT)
import Data.Either as Either
import Data.Int (floor)
import Data.List (List(..), (:), length)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.PQueue as PQueue
import Data.Set (fromFoldable)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Draw (drawAStar, inverseView, inverseViewScale)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Ref (Ref, new, read, write)
import Event.MouseEvent (MouseEvent, clientCoordinates, fromEvent, movementCoordinates, buttons)
import Graphics.Canvas (Transform)
import Partial.Unsafe (unsafePartial)
import UI (Elements, Emitted(..), producer)
import Web.DOM.Element (setClassName)
import Web.Event.Event (Event)

type ProcessInput
  = { iteration :: Int
    , world :: Transform
    , initialState :: AStarState
    , loop :: Boolean
    , ms :: Number
    }

type ProcessOutput
  = { state :: AStarState
    , result :: AStarResult
    , iteration :: Int
    }

type Process
  = { fiber :: Fiber Unit
    , input :: ProcessInput
    , output :: Ref (Maybe ProcessOutput)
    }

data Mode
  = Drawing
  | Erasing
  | Moving

type AppState
  = { playing :: Boolean
    , mode :: Mode
    , drawProcess :: Process
    }

type AppEffect
  = StateT AppState Aff

initialAStarState :: AStarState
initialAStarState =
  { statesExpanded: 0
  , maxFrontier: 0
  , explored: Set.empty
  , frontierSet: Set.empty
  , parents: Map.empty
  , frontier: PQueue.empty
  , pastExplored: Nil
  , goalTiles: (Tile 1 5) : Nil
  , foundGoalTile: Nothing
  , startTile: Tile 0 0
  , heuristic: manhattan
  , map: fromFoldable ((Tile 3 1) : (Tile 2 1) : (Tile 1 1) : (Tile 0 1) : (Tile (-1) 1) : Nil)
  }

draw :: { state :: AStarState, result :: AStarResult, iteration :: Int, world :: Transform } -> Effect Unit
draw { state, result, iteration, world } = do
  res <-
    drawAStar
      { state
      , result
      -- prevent out of bounds iteration count
      , iteration: clamp 0 (length state.pastExplored - 1) iteration
      , world
      -- renderPath after final iteration
      , renderPath: iteration > length state.pastExplored - 1
      }
  case res of
    Either.Left e -> log $ show e
    Either.Right _ -> pure unit

makeDrawProcess :: ProcessInput -> Ref (Maybe ProcessOutput) -> Aff Unit
makeDrawProcess params@{ ms, iteration, world, initialState, loop } output = do
  let
    Tuple result state = runState astar initialState
  go state result iteration
  where
  go state result iteration = do
    liftEffect $ draw { state, result, iteration, world }
    liftEffect $ write (Just { state, result, iteration }) output
    delay (Milliseconds ms)
    when loop $ go state result (iteration + 1)

killProcess :: Process -> Aff (Maybe ProcessOutput)
killProcess process = do
  killFiber (error "Error killing fiber") process.fiber
  liftEffect $ read process.output

spawnDrawProcess :: ProcessInput -> Effect Process
spawnDrawProcess input = do
  output <- liftEffect $ new Nothing
  fiber <- launchAff $ makeDrawProcess input output
  pure { fiber, input, output }

modifydrawProcess :: (ProcessInput -> ProcessInput) -> AppEffect Unit
modifydrawProcess modifyInput = do
  { drawProcess } <- get
  output <- lift $ killProcess drawProcess
  case output of
    Just { iteration } -> do
      process <- lift $ liftEffect $ spawnDrawProcess ((modifyInput drawProcess.input) { iteration = iteration })
      void $ modify _ { drawProcess = process }
    Nothing -> do
      process <- lift $ liftEffect $ spawnDrawProcess (modifyInput drawProcess.input)
      void $ modify _ { drawProcess = process }

handlePlay :: Elements -> Event -> AppEffect Unit
handlePlay { play } event = do
  { playing, drawProcess } <- modify \state -> state { playing = not state.playing }
  liftEffect $ setClassName (if playing then "pause" else "play") play
  output <- lift $ killProcess drawProcess
  modifydrawProcess _ { loop = playing }

changeMap :: MouseEvent -> (Tile -> AStarMap -> AStarMap) -> AppEffect Unit
changeMap mouseEvent modifyMap = do
  { drawProcess } <- get
  inverseCoordinates <- lift $ liftEffect $ inverseView $ clientCoordinates mouseEvent
  let
    world = drawProcess.input.world
  let
    tile = Tile (floor $ inverseCoordinates.x - world.m31) (floor $ inverseCoordinates.y - world.m32)
  let
    map = modifyMap tile drawProcess.input.initialState.map
  modifydrawProcess _ { initialState { map = map } }

transformWorld :: MouseEvent -> AppEffect Unit
transformWorld mouseEvent = do
  { drawProcess } <- get
  inverseCoordinates <- lift $ liftEffect $ inverseViewScale $ movementCoordinates mouseEvent
  let
    world = drawProcess.input.world
  let
    transformedWorld = world { m31 = world.m31 + inverseCoordinates.x, m32 = world.m32 + inverseCoordinates.y }
  modifydrawProcess _ { world = transformedWorld }

handleMouseMove :: Elements -> Event -> AppEffect Unit
handleMouseMove { canvas } event = do
  { mode, drawProcess } <- get
  let
    mouseEvent = unsafePartial $ fromJust $ fromEvent event
  when (buttons mouseEvent == 1) do
    case mode of
      Moving -> transformWorld mouseEvent
      Drawing -> changeMap mouseEvent Set.insert
      Erasing -> changeMap mouseEvent Set.delete

handleReset :: AppEffect Unit
handleReset = do
  { drawProcess } <- get
  void $ lift $ killProcess drawProcess
  process <- lift $ liftEffect $ spawnDrawProcess drawProcess.input { iteration = 0, initialState = initialAStarState }
  void $ modify _ { drawProcess = process }

reduce :: Emitted -> AppEffect Unit
reduce emitted = case emitted of
  Play elements event -> handlePlay elements event
  Draw elements event -> void $ modify _ { mode = Drawing }
  Erase elements event -> void $ modify _ { mode = Erasing }
  Move elements event -> void $ modify _ { mode = Moving }
  Reset elements event -> handleReset
  MouseMove elements event -> handleMouseMove elements event

main :: Effect Unit
main = do
  let
    world = { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
  let
    input = { initialState: initialAStarState, iteration: 0, world, loop: false, ms: 10.0 }
  drawProcess <- spawnDrawProcess input
  let
    initialAppState = { playing: false, mode: Drawing, drawProcess }
  startUI (makeConsumer reduce) initialAppState

makeConsumer :: forall m. MonadAff m => (Emitted -> m Unit) -> Consumer Emitted m Unit
makeConsumer changeState = forever $ lift <<< changeState =<< await

startUI :: Consumer Emitted AppEffect Unit -> AppState -> Effect Unit
startUI consumer = launchAff_ <<< (runStateT $ runProcess $ consumer `pullFrom` producer)
