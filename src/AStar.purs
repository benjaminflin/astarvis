module AStar where

import Prelude

import Data.AffStream (Stream, (>>-), fromCallback)
import Control.Apply (lift2)
import Control.Monad.Rec.Loops (whileM_)
import Control.MonadZero (guard)
import Control.Monad.RWS (RWST, evalRWST, ask, get, gets, modify_)
import Data.Array (filter, fromFoldable)
import Data.Tuple (snd)
import Data.Int (toNumber)
import Data.Foldable (all, foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (choose, isRight, Either(..))
import Data.List.Types (NonEmptyList)
import Data.List.NonEmpty as N
import Data.PQueue (PQueue)
import Data.PQueue as Q
import Data.Set (Set) 
import Data.Set as S
import Data.Newtype (class Newtype, unwrap, over)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Debug.Trace (traceM)

data Tile = Tile Int Int
instance showTile :: Show Tile where
    show (Tile x y) = "(" <> show x <> ", " <> show y <> ")"
derive instance eqTile :: Eq Tile
derive instance ordTile :: Ord Tile

type TileSet = Set Tile 
type Trail = NonEmptyList Tile

newtype Path 
    = Path 
    { cost :: Number
    , trail :: Trail
    }
derive instance newtypePath :: Newtype Path _
derive instance eqPath :: Eq Path
instance ordPath :: Ord Path where
    compare (Path a) (Path b) = compare a.cost b.cost

instance showPath :: Show Path where
    show (Path { trail, cost }) = show (fromFoldable trail) <> " " <> show cost

type Frontier = 
    { queue :: PQueue Number Path 
    , set :: Set Tile 
    }

type Params
    = { map :: TileSet
      , start :: Tile
      , goal :: Tile
      , heuristic :: Tile -> Tile -> Number
      }

type State
    = { explored :: TileSet
      , frontier :: Frontier
      }

type Result = Maybe Path
type AStar a = RWST Params Unit State Aff a 

appendTile :: Number -> Tile -> Path -> Path
appendTile c t = over Path (\p -> p { cost = p.cost + c + depth p, trail = N.cons t p.trail }) 
    where depth p = toNumber $ N.length p.trail + 1

tile :: Path -> Tile
tile = N.head <<< _.trail <<< unwrap

front :: Frontier -> Maybe Path
front = map snd <<< Q.head <<< (_.queue)

back :: Frontier -> Frontier
back f 
    = f { queue = fromMaybe Q.empty $ Q.tail f.queue
        , set = fromMaybe f.set $ s' 
        }
    where
    s' = flip S.delete f.set <<< tile <$> front f

push :: Path -> Frontier -> Frontier
push p'@(Path p) f 
    = f { queue = Q.insert p.cost p' f.queue
        , set = S.insert (N.head p.trail) f.set 
        }

succs :: Tile -> Array Tile
succs (Tile x y) 
    = [ Tile (x+1) y
      , Tile (x-1) y
      , Tile x (y+1)
      , Tile x (y-1)
      ] 

explore :: Path -> AStar Unit
explore p = do
    { map, heuristic, goal } <- ask
    { explored, frontier } <- get
    let check t = all (_ $ t) $ not <<< flip S.member <$> [map, explored, frontier.set]
        succs' = filter check <<< succs <<< tile $ p
        paths = flip (appendTile =<< heuristic goal) p <$> succs' 
    modify_ _ { frontier = foldl (flip push) frontier paths }    

step :: AStar (Either Result State)
step = do
    { goal } <- ask
    path <- gets $ front <<< _.frontier
    let explore' = lift2 (<*) modifyState explore <$> path 
    st <- maybe (pure Nothing) ((Just <$> get) <* _) explore'
    let res = guard' ((_ == goal) <<< tile) path
    pure $ toEither res st 
    where 
        modifyState p = modify_ $ \s -> 
                            s { frontier = back s.frontier
                              , explored = S.insert (tile p) s.explored
                              }
        guard' f = (=<<) $ lift2 (<*) pure (guard <<< f)
        toEither (Just r) _ = Left $ Just r 
        toEither _ (Just s) = Right s
        toEither _ _ = Left Nothing 

astarS :: Stream Params -> Stream (Either Result State)
astarS s = let bind = (>>-) in do 
    params <- s 
    fromCallback $ eval params <<< whileM' <<< step'
    where
    initialState p = { frontier: { queue: Q.singleton cost 
                                            $ Path { trail: N.singleton p.start, cost }
                                 , set: S.singleton p.start 
                                 }
                     , explored: S.empty
                     }
                     where cost = p.heuristic p.start p.goal
    eval p a = void $ evalRWST a p $ initialState p
    whileM' = flip whileM_ (pure unit)
    step' emit = lift2 (<*) (pure <<< isRight) (liftAff <<< emit) =<< step 


