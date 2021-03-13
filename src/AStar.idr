module AStar


import Control.Monad.RWS

import Data.SortedSet
import Data.Vect

Tile : Type
Tile = Vect 2 Integer


record Path where
  constructor MkPath
  cost : Double
  trail : Vect (S n) Tile 

Eq Path where
  (==) p1 p2 = p1.cost == p2.cost

Ord Path where
  compare p1 p2 = compare p1.cost p2.cost

record Frontier where
  constructor MkFrontier
  queue : List Path 
  set : SortedSet Tile

record Params where
  constructor MkParams
  map : SortedSet Tile
  start : Tile
  goal : Tile
  heuristic : Tile -> Tile -> Double

record State where
  constructor MkState
  explored : SortedSet Tile
  frontier : Frontier 

data Result
  = FoundPath Path
  | InProgress
  | NoPath 

AStar : Type -> Type
AStar = RWST Params () State IO


appendTile : Double -> Tile -> Path -> Path
appendTile d t p = MkPath d (t::p.trail)

succs : Tile -> Vect 4 Tile
succs [x, y] =
  [ [x+1, y]
  , [x-1, y]
  , [x, y+1]
  , [x, y-1]
  ]


queueInsert : Path -> List Path -> List Path
queueInsert p [] = [p] 
queueInsert p (x :: xs) = 
  if p < x then p :: x :: xs else x :: queueInsert p xs

push : Path -> Frontier -> Frontier
push p f 
    = record 
        { queue = queueInsert p f.queue 
        , set = insert (head p.trail) f.set 
        } f

explore : Path -> AStar ()
explore path = do
  params <- ask 
  state <- get
  let ps = paths params state
  put $ record { frontier = foldl (flip push) state.frontier (snd ps) } state

  where

  check : Params -> State -> Tile -> Bool 
  check params state t = 
    let (::) = (Data.Vect.(::)) in 
      all id $ (not . contains t) <$> [ params.map
                                      , state.frontier.set
                                      , state.explored ]
  succs' : Params -> State -> (p ** Vect p Tile)
  succs' params state = 
    filter (check params state) (succs . Data.Vect.head $ path.trail)

  paths : Params -> State -> (p ** Vect p Path)
  paths params state = 
    let (n ** v) = succs' params state in
        (n ** (\t => appendTile (params.heuristic params.goal t) t path) <$> v)


step : AStar Result 
step = do
  st <- get
  case st.frontier.queue of
    Nil => pure NoPath
    (curPath :: otherPaths) => do
      modify (\st => record { frontier.queue = otherPaths
                            , frontier.set = delete (head curPath.trail) st.frontier.set 
                            } st)
      if curPath.cost < 0.1 then pure (FoundPath curPath) else 
        do
          explore curPath
          pure InProgress


euclidean : Tile -> Tile -> Double
euclidean [x, y] [x', y'] = fromInteger $ ((x - x') * (x - x')) + ((y - y') * (y - y'))

exParams : Params
exParams = MkParams (fromList []) [0,0] [0,5] euclidean

test : IO (List Path)
test = do
  (s, _) <- execRWST step exParams (MkState (fromList []) (MkFrontier [(MkPath 5.0 [[0,0]])] (fromList [[0,0]])))
  pure $ s.frontier.queue 


