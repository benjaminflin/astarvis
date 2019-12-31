module AStar where

import Prelude
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (close, emit, produce')
import Control.Monad.Loops (whileM_)
import Control.Monad.Maybe.Trans (lift)
import Control.Monad.RWS (RWST, ask, evalRWST)
import Control.Monad.State (get, modify_)
import Data.Either as Either
import Data.Int (toNumber)
import Data.List (List(..), filter, foldl, (:))
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList, toList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (abs)
import Data.PQueue as PQueue
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Global (infinity)

data Action
  = Up
  | Down
  | Left
  | Right
  | None

derive instance eqAction :: Eq Action

data Tile
  = Tile Int Int

instance showTile :: Show Tile where
  show (Tile x y) = "Tile " <> show x <> ", " <> show y

derive instance eqTile :: Eq Tile

derive instance ordTile :: Ord Tile

type AStarMap
  = Set.Set Tile

type Frontier
  = Tuple (PQueue.PQueue Number FrontierItem) (Set.Set Tile)

type Explored
  = Set.Set Tile

type Path
  = NonEmptyList (Tuple Tile Action)

newtype FrontierItem
  = FrontierItem
  { cost :: Number
  , path :: Path
  }

derive instance eqFrontierItem :: Eq FrontierItem

instance ordFrontierItem :: Ord FrontierItem where
  compare (FrontierItem a) (FrontierItem b) = compare a.cost b.cost

type AStarState
  = { statesExpanded :: Int
    , maxFrontier :: Int
    , explored :: Explored
    , frontier :: Frontier
    }

type AStarParams
  = { map :: AStarMap
    , startTile :: Tile
    , goalTiles :: NonEmptyList Tile
    , heuristic :: NonEmptyList Tile -> Tile -> Number
    }

type AStarResult
  = Maybe Path

type AStarEffect a
  = RWST AStarParams Unit AStarState Aff a

front :: Frontier -> Maybe FrontierItem
front (Tuple queue _) = snd <$> PQueue.head queue

push :: Frontier -> FrontierItem -> Frontier
push (Tuple queue set) (FrontierItem i) = Tuple (PQueue.insert i.cost (FrontierItem i) queue) (Set.insert (head (FrontierItem i)) set)

member :: Tile -> Frontier -> Boolean
member i (Tuple _ set) = Set.member i set

head :: FrontierItem -> Tile
head (FrontierItem i) = fst $ NonEmptyList.head i.path

goalTest :: NonEmptyList Tile -> Tile -> Boolean
goalTest goals tile = foldl (\found goal -> (tile == goal) || found) false goals

back :: Frontier -> Frontier
back (Tuple queue set) = Tuple (fromMaybe PQueue.empty $ PQueue.tail queue) (maybe set (flip Set.delete set) (head <$> snd <$> PQueue.head queue))

size :: Frontier -> Int
size (Tuple _ set) = Set.size set

init :: AStarEffect Unit
init = do
  { startTile, heuristic, goalTiles } <- ask
  let
    item =
      FrontierItem
        { cost: heuristic goalTiles startTile
        , path: NonEmptyList.singleton (Tuple startTile None)
        }
  modify_ \state -> state { frontier = push state.frontier item }

successors :: Tile -> List (Tuple Tile Action)
successors (Tile x y) = (makePair (x - 1) y Left) : (makePair (x + 1) y Right) : (makePair x (y + 1) Up) : (makePair x (y - 1) Down) : Nil
  where
  makePair x' y' a = Tuple (Tile x' y') a

explore :: FrontierItem -> AStarEffect Unit
explore (FrontierItem item) = do
  { map, heuristic, goalTiles } <- ask
  { explored, frontier } <- get
  let
    check = \tile -> not (Set.member tile map) && not (Set.member tile explored) && not (member tile frontier)
  let
    succs =
      filter
        (check <<< fst)
        (successors $ head (FrontierItem item))
  let
    items = (\succ -> FrontierItem { cost: (heuristic goalTiles (fst succ)) + (toNumber $ NonEmptyList.length item.path + 1), path: (NonEmptyList.cons succ item.path) }) <$> succs
  modify_ _ { frontier = foldl push frontier items }

step :: AStarEffect (Either.Either Explored AStarResult)
step = do
  { goalTiles } <- ask
  { frontier } <- get
  case front frontier of
    Just (FrontierItem item) -> do
      let
        tile = head (FrontierItem item)
      if goalTest goalTiles tile then
        pure (Either.Right $ Just item.path)
      else do
        modify_ \state ->
          state
            { frontier = back frontier
            , maxFrontier = max state.maxFrontier $ size frontier
            , explored = Set.insert tile state.explored
            }
        explore (FrontierItem item)
        { explored } <- get
        pure $ Either.Left explored
    Nothing -> pure $ Either.Right Nothing

astar :: forall m. MonadAff m => AStarParams -> Producer Explored m AStarResult
astar params =
  let
    initialState = { statesExpanded: 0, maxFrontier: 0, explored: Set.empty, frontier: Tuple PQueue.empty Set.empty }

    go emitter = do
      init
      whileM_
        ( step
            >>= case _ of
                Either.Left explored -> do
                  lift $ liftEffect $ emit emitter explored
                  pure true
                Either.Right result -> do
                  lift $ liftEffect $ close emitter result
                  pure false
        )
        (pure unit)
  in
    produce' \emitter ->
      launchAff_
        $ evalRWST
            (go emitter)
            params
            initialState

manhattan :: NonEmptyList Tile -> Tile -> Number
manhattan neList tile = go (toList neList) tile
  where
  go ((Tile xGoal yGoal) : rest) (Tile x y) = min (toNumber (abs (x - xGoal) + abs (y - yGoal))) (go rest (Tile x y))

  go Nil _ = infinity
