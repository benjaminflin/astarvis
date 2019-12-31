module Main where

import Prelude
import AStar (AStarMap, AStarParams, AStarResult, Tile(..), Explored, astar, manhattan)
import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, modify, runStateT)
import Control.Monad.State.Trans (lift, StateT)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.List (List(..), (:))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Set (fromFoldable)
import Data.Set as Set
import Draw (drawAStar, inverseView, inverseViewScale)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref, new, read)
import Effect.Ref as Ref
import Event.MouseEvent (MouseEvent, clientCoordinates, fromEvent, movementCoordinates, buttons)
import Graphics.Canvas (Transform)
import Partial.Unsafe (unsafePartial)
import UI (Elements, Emitted(..))
import UI as UI
import Web.DOM.Element (setClassName)
import Web.Event.Event (Event)

type ProcessInput
  = { iteration :: Int
    , world :: Transform
    , params :: AStarParams
    , loop :: Boolean
    , ms :: Number
    }

type ProcessOutput
  = Int

type Process
  = { fiber :: Fiber Unit
    , input :: ProcessInput
    , output :: Ref ProcessOutput
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

astarParams :: AStarParams
astarParams =
  { goalTiles: singleton $ Tile 5 5
  , startTile: Tile 0 0
  , heuristic: manhattan
  , map: fromFoldable ((Tile 3 1) : (Tile 2 1) : (Tile 1 1) : (Tile 0 1) : (Tile (-1) 1) : Nil)
  }

makeDrawProcess :: ProcessInput -> Ref ProcessOutput -> Aff Unit
makeDrawProcess { ms, iteration, world, params, loop } output = do
  let
    producer :: Producer Explored Aff AStarResult
    producer = astar params

    waitUntil :: Int -> Consumer Explored Aff Explored
    waitUntil n
      | n > 0 = do
        _ <- await
        waitUntil (n - 1)
      | otherwise = await

    consumeForever :: Consumer Explored Aff AStarResult
    consumeForever = do
      void $ waitUntil iteration
      forever do
        explored <- await
        void $ lift $ liftEffect $ drawAStar { params, world, toDraw: Left explored }
        lift $ delay (Milliseconds ms)
        void $ lift $ liftEffect $ Ref.modify (_ + 1) output

    consumeOnce :: Consumer Explored Aff AStarResult
    consumeOnce = do
      explored <- waitUntil (iteration + 1)
      void $ lift $ liftEffect $ drawAStar { params, world, toDraw: Left explored }
      pure Nothing
  result <- runProcess ((if loop then consumeForever else consumeOnce) `pullFrom` producer)
  when (isJust result) do
    void $ liftEffect $ drawAStar { params, world, toDraw: Right result }

killProcess :: Process -> Aff ProcessOutput
killProcess process = do
  killFiber (error "Error killing fiber") process.fiber
  liftEffect $ read process.output

spawnDrawProcess :: ProcessInput -> Effect Process
spawnDrawProcess input = do
  output <- new input.iteration
  fiber <- launchAff $ makeDrawProcess input output
  pure { fiber, input, output }

modifydrawProcess :: (ProcessInput -> ProcessInput) -> AppEffect Unit
modifydrawProcess modifyInput = do
  { drawProcess } <- get
  iteration <- lift $ killProcess drawProcess
  process <- lift $ liftEffect $ spawnDrawProcess ((modifyInput drawProcess.input) { iteration = iteration })
  void $ modify _ { drawProcess = process }

handlePlay :: Elements -> Event -> AppEffect Unit
handlePlay { play } event = do
  { playing, drawProcess } <- modify \state -> state { playing = not state.playing }
  liftEffect $ setClassName (if playing then "pause" else "play") play
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
    map = modifyMap tile drawProcess.input.params.map
  modifydrawProcess _ { params { map = map } }

transformWorld :: MouseEvent -> AppEffect Unit
transformWorld mouseEvent = do
  { drawProcess } <- get
  inverseCoordinates <- lift $ liftEffect $ inverseViewScale $ movementCoordinates mouseEvent
  let
    world = drawProcess.input.world
  let
    transformedWorld = world { m31 = world.m31 + inverseCoordinates.x, m32 = world.m32 + inverseCoordinates.y }
  modifydrawProcess _ { world = transformedWorld }

handleMouseMove :: Event -> AppEffect Unit
handleMouseMove event = do
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
  process <- lift $ liftEffect $ spawnDrawProcess drawProcess.input { iteration = 0, params = astarParams }
  void $ modify _ { drawProcess = process }

reduce :: Emitted -> AppEffect Unit
reduce emitted = case emitted of
  Play elements event -> handlePlay elements event
  Draw _ _ -> void $ modify _ { mode = Drawing }
  Erase _ _ -> void $ modify _ { mode = Erasing }
  Move _ _ -> void $ modify _ { mode = Moving }
  Reset _ _ -> handleReset
  MouseMove _ event -> handleMouseMove event

main :: Effect Unit
main = do
  let
    world = { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
  let
    input = { params: astarParams, iteration: 0, world, loop: false, ms: 20.0 }
  drawProcess <- spawnDrawProcess input
  let
    initialAppState = { playing: false, mode: Drawing, drawProcess }
  startUI (makeConsumer reduce) initialAppState

makeConsumer :: forall m. MonadAff m => (Emitted -> m Unit) -> Consumer Emitted m Unit
makeConsumer changeState = forever $ lift <<< changeState =<< await

startUI :: Consumer Emitted AppEffect Unit -> AppState -> Effect Unit
startUI consumer = launchAff_ <<< (runStateT $ runProcess $ consumer `pullFrom` UI.producer)
