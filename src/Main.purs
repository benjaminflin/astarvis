module Main where

import Prelude
import Effect (Effect)
import Data.Map as Map
import Data.Set as Set
import Data.PQueue as Queue
import AStar (AStarState(..), Tile(..), astar, manhattan)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Control.Monad.State (runState)
import Draw (drawAStar)
import Data.Either as Either

initialState :: AStarState
initialState =
  ( AStarState
      { statesExpanded: 0
      , maxFrontier: 0
      , explored: Set.empty
      , frontierSet: Set.empty
      , parents: Map.empty
      , frontier: Queue.empty
      , pastFrontiers: Nil
      , goalTiles: (Tile 1 5) : Nil
      , foundGoalTile: Nothing
      , startTile: Tile 0 0
      , heuristic: manhattan
      , map: Map.empty
      }
  )

main :: Effect Unit
main = do
  let
    (Tuple result state) = runState astar initialState
  cmp <- drawAStar
  case cmp of
    Either.Left e -> log $ show e
    Either.Right _ -> pure unit
  log (show result)
