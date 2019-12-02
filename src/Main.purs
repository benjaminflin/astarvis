module Main where

import Prelude
import Control.Monad.Loops (whileM_)
import Control.Monad.State.Trans (lift, StateT)
import Control.Monad.State (runState, modify, get)
import Data.Map as Map
import Data.Int (floor)
import Data.Set as Set
import Data.PQueue as Queue
import Data.List (List(..), (:), length)
import Data.Maybe (Maybe(..))
import Data.Either as Either
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff
import Effect.Ref
import Effect.Exception (error)
import Event.MouseEvent (ClickEvent, fromEvent, toEvent, clientCoordinates)
import Graphics.Canvas (Transform(..))
import Web.DOM.Element (setClassName)
import Web.Event.Event (Event)
import AStar (AStarState(..), AStarResult, Tile(..), astar, manhattan)
import Draw (drawAStar, inverseView)
import UI
import Debug.Trace

insertMany :: List Tile -> Set.Set Tile -> Set.Set Tile
insertMany (t : ts) set = Set.insert t (insertMany ts set)

insertMany Nil set = set

initialState :: AStarState
initialState =
  ( AStarState
      { statesExpanded: 0
      , maxFrontier: 0
      , explored: Set.empty
      , frontierSet: Set.empty
      , parents: Map.empty
      , frontier: Queue.empty
      , pastExplored: Nil
      , goalTiles: (Tile 1 5) : Nil
      , foundGoalTile: Nothing
      , startTile: Tile 0 0
      , heuristic: manhattan
      , map: insertMany ((Tile 3 1) : (Tile 2 1) : (Tile 1 1) : (Tile 0 1) : (Tile (-1) 1) : Nil) Set.empty
      }
  )

loopDraw :: AStarState -> AStarResult -> Ref Int -> Ref Transform -> Aff Unit
loopDraw state result refIter refWorld =
  whileM_
    ( do
        let
          (AStarState s) = state
        i <- liftEffect $ read refIter
        world <- liftEffect $ read refWorld
        let
          (AStarState s) = state
        when (i >= length s.pastExplored - 1) $ liftEffect $ draw state result i true world
        pure $ i <= length s.pastExplored - 1
    )
    $ do
        i <- liftEffect $ read refIter
        world <- liftEffect $ read refWorld
        liftEffect $ draw state result i false world
        delay (Milliseconds 10.0)
        liftEffect $ write (i + 1) refIter

draw :: AStarState -> AStarResult -> Int -> Boolean -> Transform -> Effect Unit
draw state result iterations renderPath world = do
  let
    (AStarState s) = state
  res <-
    drawAStar
      { state
      , result
      , iterations: clamp 0 (length s.pastExplored - 1) iterations
      , world
      , renderPath
      }
  case res of
    Either.Left e -> log $ show e
    Either.Right _ -> pure unit

data DrawMode
  = Drawing
  | Erasing

data Mode
  = Pen DrawMode
  | Moving

data MouseState
  = Down
  | Up

type AppState
  = { playing :: Boolean
    , drawProcess :: Maybe (Fiber Unit)
    , refIter :: Ref Int
    , refWorld :: Ref Transform
    , initialState :: AStarState
    , mode :: Mode
    , mouseState :: MouseState
    }

type AppEffect
  = StateT AppState Aff

handlePlay :: Elements -> Event -> AppEffect Unit
handlePlay { playButton } ev = do
  -- set playing = not playing
  { playing, refIter, initialState, refWorld } <- modify \s@{ playing } -> s { playing = not playing }
  if playing then do
    -- spawn draw process and update state
    let
      Tuple result state = runState astar initialState
    fiber <- lift $ forkAff $ loopDraw state result refIter refWorld
    void $ modify \s@{ drawProcess } -> s { drawProcess = Just fiber }
    -- change play button icon
    liftEffect $ setClassName "pause" playButton
  else do
    -- kill draw process
    { drawProcess } <- get
    case drawProcess of
      Just fiber -> lift $ killFiber (error "Error killing fiber") fiber
      _ -> pure unit
    -- change play button icon
    liftEffect $ setClassName "play" playButton

handleReset :: Elements -> Event -> AppEffect Unit
handleReset { playButton } ev = do
  { refIter, initialState, drawProcess, refWorld } <- get
  -- kill the draw process
  case drawProcess of
    Just fiber -> lift $ killFiber (error "Error killing fiber") fiber
    _ -> pure unit
  void $ modify \s@{ playing } -> s { playing = false }
  -- change play button icon
  lift $ liftEffect $ setClassName "play" playButton
  -- set i to 0
  lift $ liftEffect $ write 0 refIter
  -- draw initialState
  let
    Tuple result state = runState astar initialState
  world <- lift $ liftEffect $ read refWorld
  lift $ liftEffect $ draw state result 0 false world

-- set the mode to drawing
handleDraw :: Elements -> Event -> AppEffect Unit
handleDraw _ ev = void $ modify \s -> s { mode = Pen Drawing }

-- set the mode to erasing
handleErase :: Elements -> Event -> AppEffect Unit
handleErase _ ev = void $ modify \s -> s { mode = Pen Erasing }

handleCanvas :: Elements -> Event -> AppEffect Unit
handleCanvas { canvas } ev = do
  { mouseState } <- get
  when
    ( case mouseState of
        Down -> true
        Up -> false
    )
    $ do
        case fromEvent ev of
          Just clickEvent -> do
            let
              { x, y } = clientCoordinates clickEvent
            { mode, refWorld, initialState, refIter, playing, drawProcess } <- get
            world <- lift $ liftEffect $ read refWorld
            case mode of
              Moving -> pure unit
              Pen drawMode -> do
                -- get the tile coordinates
                { x, y } <- lift $ liftEffect $ inverseView { x, y }
                let
                  (AStarState as) = initialState
                let
                  tile = Tile (floor x) (floor y)
                let
                  operation = case drawMode of
                    Drawing -> Set.insert
                    Erasing -> Set.delete
                -- add/remove tile
                { initialState } <- modify \s -> s { initialState = AStarState (as { map = operation tile as.map }) }
                let
                  Tuple result state = runState astar initialState
                i <- lift $ liftEffect $ read refIter
                -- kill the drawing process and restart it if necessary
                case drawProcess of
                  Just fiber -> lift $ killFiber (error "Error killing fiber") fiber
                  _ -> pure unit
                if playing then do
                  fiber <- lift $ forkAff $ loopDraw state result refIter refWorld
                  void $ modify \s@{ drawProcess } -> s { drawProcess = Just fiber }
                else
                  lift $ liftEffect $ draw state result i false world
          Nothing -> pure unit

changeState :: Emitted -> AppEffect Unit
changeState em = do
  case em of
    Canvas _ MouseDown _ -> void $ modify \s -> s { mouseState = Down }
    Canvas els MouseMove ev -> handleCanvas els ev
    Canvas _ MouseUp _ -> void $ modify \s -> s { mouseState = Up }
    Play els ev -> handlePlay els ev
    Reset els ev -> handleReset els ev
    Draw els ev -> handleDraw els ev
    Erase els ev -> handleErase els ev
    _ -> pure unit

main :: Effect Unit
main = do
  let
    Tuple result state = runState astar initialState
  refIter <- new 0
  let
    world = { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
  refWorld <- new world
  draw state result 0 false world
  let
    initialAppState = { playing: false, drawProcess: Nothing, refIter, refWorld, initialState, mode: Pen Drawing, mouseState: Up }
  startUI (makeConsumer changeState) initialAppState
