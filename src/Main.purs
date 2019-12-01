module Main where

import Prelude
import Control.Monad.State.Trans (lift, StateT)
import Control.Monad.State (runState, modify, get)
import Data.Map as Map
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
import Web.DOM.Element (setClassName)
import Web.DOM (Element)
import Web.Event.Event (Event)
import AStar (AStarState(..), AStarResult, Tile(..), astar, manhattan)
import Draw (drawAStar)
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

loopAStar :: Ref Int -> AStarState -> AStarResult -> Boolean -> Aff Unit
loopAStar refIter state result renderPath = do
  i <- liftEffect $ read refIter
  res <-
    liftEffect
      $ drawAStar
          { state
          , result
          , iterations: i
          , world: { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
          , renderPath
          }
  liftEffect
    $ case res of
        Either.Left e -> log $ show e
        Either.Right _ -> pure unit
  let
    (AStarState st) = state
  delay (Milliseconds 300.0)
  when (i < length st.pastExplored - 1) do
    liftEffect $ write (i + 1) refIter
    loopAStar refIter state result false
  when (i == length st.pastExplored - 1) $ loopAStar refIter state result true

type AppState
  = { playing :: Boolean
    , drawProcess :: Maybe (Fiber Unit)
    , refIter :: Ref Int
    , initialState :: AStarState
    }

type AppEffect
  = StateT AppState Aff

handlePlayClick :: Element -> Event -> AppEffect Unit
handlePlayClick el ev = do
  { playing, refIter, initialState } <- modify \s@{ playing } -> s { playing = not playing }
  if playing then do
    let
      Tuple result state = runState astar initialState
    fiber <- lift $ forkAff $ loopAStar refIter state result false
    void $ modify \s@{ drawProcess } -> s { drawProcess = Just fiber }
    liftEffect $ setClassName "pause" el
  else do
    { drawProcess } <- get
    case drawProcess of
      Just fiber -> lift $ killFiber (error "Error killing fiber") fiber
      _ -> pure unit
    liftEffect $ setClassName "play" el

changeState :: Emitted -> AppEffect Unit
changeState em = do
  case em of
    Play el ev -> handlePlayClick el ev
    _ -> pure unit

main :: Effect Unit
main = do
  refIter <- new 0
  startUI (makeConsumer changeState) ({ playing: false, drawProcess: Nothing, refIter, initialState })
