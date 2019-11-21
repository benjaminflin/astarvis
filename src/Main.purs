module Main where

import Prelude
import Effect (Effect)
import Data.Map as Map
import Data.Set as Set
import Data.PQueue as Queue
import AStar (AStarState(..), AStarResult, Tile(..), astar, manhattan)
import Data.List (List(..), (:), length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Control.Monad.State (runState)
import Draw (drawAStar)
import Data.Either as Either
import Effect.Aff
import Effect.Class

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

doAStar :: Int -> AStarState -> AStarResult -> Boolean -> Aff Unit
doAStar i state result renderPath = do
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
  delay (Milliseconds 1000.0)
  when (i < length st.pastExplored - 1) $ doAStar (i + 1) state result false
  when (i == length st.pastExplored - 1) $ doAStar i state result true

main :: Effect Unit
main = do
  let
    (Tuple result state) = runState astar initialState
  _ <- launchAff $ doAStar 0 state result false
  log (show result)
