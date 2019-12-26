module AStar where

import Prelude
import Data.List (List(..), filter, foldl, reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Map as Map
import Data.Set as Set
import Data.PQueue as Queue
import Control.Monad.State (State, get, modify_, put)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Int (toNumber)
import Data.Ord (abs)
import Global (infinity)

data Action
  = Up
  | Down
  | Left
  | Right
  | None

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"
  show None = "None"

data Tile
  = Tile Int Int

derive instance eqTile :: Eq Tile

derive instance ordTile :: Ord Tile

instance showTile :: Show Tile where
  show (Tile x y) = "Tile " <> show x <> " " <> show y

newtype FrontierItem
  = FrontierItem
  { cost :: Number
  , action :: Action
  , childTile :: Tile
  , parentTile :: Maybe Tile
  , depth :: Int
  }

derive instance eqFrontierItem :: Eq FrontierItem

instance ordFrontierItem :: Ord FrontierItem where
  compare (FrontierItem a) (FrontierItem b) = compare a.cost b.cost

type AStarMap
  = Set.Set Tile

type Frontier
  = Queue.PQueue Number FrontierItem

type AStarState
  = { statesExpanded :: Int
    , maxFrontier :: Int
    , explored :: Set.Set Tile
    , frontierSet :: Set.Set Tile
    , pastExplored :: List (Set.Set Tile)
    , parents :: Map.Map Tile (Tuple Tile Action)
    , frontier :: Frontier
    , goalTiles :: List Tile
    , foundGoalTile :: Maybe Tile
    , startTile :: Tile
    , heuristic :: List Tile -> Tile -> Number
    , map :: AStarMap
    }

type AStarResult
  = Maybe (List (Tuple Tile Action))

initFrontier :: State AStarState Unit
initFrontier = do
  state@{ heuristic, startTile, frontier, goalTiles, frontierSet } <- get
  let
    cost = heuristic goalTiles startTile
  let
    frontierItem =
      ( FrontierItem
          { cost
          , action: None
          , childTile: startTile
          , parentTile: Nothing
          , depth: 0
          }
      )
  put
    state
      { frontier = Queue.insert cost frontierItem frontier
      , frontierSet = Set.insert startTile frontierSet
      }

getSuccessors :: Tile -> AStarMap -> List (Tuple Tile Action)
getSuccessors (Tile x y) map =
  snd
    <$> filter (fst)
        ( wall (x + 1) y Right
            : wall (x - 1) y Left
            : wall x (y + 1) Up
            : wall x (y - 1) Down
            : Nil
        )
  where
  wall x y a = Tuple (not $ Set.member (Tile x y) map) (Tuple (Tile x y) a)

pushFrontier :: FrontierItem -> State AStarState Unit
pushFrontier (FrontierItem head) = do
  state@{ map, goalTiles, heuristic, frontier, explored, frontierSet } <- get
  let
    succs = (filter (fst >>> \tile -> not (Set.member tile explored) && not (Set.member tile frontierSet)) (getSuccessors head.childTile map))
  let
    newFrontier =
      foldl
        ( \frontier (Tuple tile action) ->
            let
              cost = 3.0 * (heuristic goalTiles tile) + (toNumber head.depth + 1.0)

              frontierItem =
                ( FrontierItem
                    { cost
                    , action
                    , childTile: tile
                    , parentTile: Just head.childTile
                    , depth: head.depth + 1
                    }
                )
            in
              (Queue.insert cost frontierItem frontier)
        )
        frontier
        succs
  let
    newFrontierSet = foldl (flip Set.insert) frontierSet (fst <$> succs)
  put
    state
      { frontier = newFrontier
      , frontierSet = newFrontierSet
      }

loop :: State AStarState Unit
loop = do
  state@{ frontier, frontierSet, parents, goalTiles, statesExpanded, explored, maxFrontier, pastExplored } <- get
  Queue.head frontier
    `maybeDo`
      \(Tuple _ (FrontierItem head)) -> do
        let
          goalTile = if (isGoal goalTiles head.childTile) then Just head.childTile else Nothing

          newParents = case head.parentTile of
            Just tile -> Map.insert (head.childTile) (Tuple tile head.action) parents
            Nothing -> parents
        put
          state
            { frontierSet = Set.delete head.childTile frontierSet
            , frontier = fromMaybe Queue.empty (Queue.tail frontier)
            , parents = newParents
            , foundGoalTile = goalTile
            , pastExplored = explored : pastExplored
            }
        when (isNothing goalTile)
          $ do
              state <- get
              put
                state
                  { statesExpanded = statesExpanded + 1
                  , explored = Set.insert head.childTile explored
                  , maxFrontier = max (Set.size frontierSet) maxFrontier
                  }
              pushFrontier (FrontierItem head)
              loop
        when (isJust goalTile) $ modify_ (\s -> s { pastExplored = reverse s.pastExplored })
  where
  maybeDo = flip $ maybe (pure unit)

  isGoal (x : xs) tile = if tile == x then true else isGoal xs tile

  isGoal Nil _ = false

genPath :: State AStarState AStarResult
genPath = do
  state@{ parents, foundGoalTile } <- get
  case maybe (Nil) (findPath parents Nil) foundGoalTile of
    Nil -> pure Nothing
    path -> pure (Just path)
  where
  findPath parents path tile = case Map.lookup tile parents of
    Just (Tuple t a) -> findPath parents ((Tuple t a) : path) t
    Nothing -> path

astar :: State AStarState AStarResult
astar = do
  initFrontier
  loop
  genPath

manhattan :: List Tile -> Tile -> Number
manhattan ((Tile xGoal yGoal) : rest) (Tile x y) = min (toNumber (abs (x - xGoal) + abs (y - yGoal))) (manhattan rest (Tile x y))

manhattan ((Tile xGoal yGoal) : Nil) (Tile x y) = toNumber $ abs (x - xGoal) + abs (y - yGoal)

manhattan Nil _ = infinity
