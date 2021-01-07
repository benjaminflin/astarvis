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
import Data.HashSet (HashSet) 
import Data.HashSet as S
import Data.Hashable 
import Data.Typelevel.Num (D2, d0, d1) 
import Data.Vec (Vec, toArray, (!!), vec2, dotProduct)
import Data.Newtype (class Newtype, unwrap, over)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Debug.Trace (traceM)

type Set = HashSet
newtype Tile = Tile (Vec D2 Int)

derive instance newtypeTile :: Newtype Tile _
instance hashableEq :: Hashable Tile where
    hash = hash <<< toArray <<< unwrap
derive newtype instance tileEq :: Eq Tile 
derive newtype instance tileShow :: Show Tile

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
succs (Tile t)
    = Tile <$> 
      [ vec2 (x+1) y
      , vec2 (x-1) y
      , vec2 x (y+1)
      , vec2 x (y-1)
      ] 
      where x = t !! d0
            y = t !! d1

explore :: Path -> AStar Unit
explore p = do
    { map, heuristic, goal } <- ask
    { explored, frontier } <- get
    let check t = all (_ $ t) $ not <<< flip S.member <$> [map, explored, frontier.set]
        succs' = filter check <<< succs <<< tile $ p
        paths = flip (appendTile =<< heuristic goal) p <$> succs' 
    traceM $ show $ succs $ tile p
    modify_ _ { frontier = foldl (flip push) frontier paths }    

step :: AStar (Either Result State)
step = do
    { goal } <- ask
    path <- gets $ front <<< _.frontier
    let explore' = lift2 (<*) modifyState explore <$> path 
        res = guard' ((_ == goal) <<< tile) path
    st <- maybe (pure Nothing) ((Just <$> get) <* _) explore'
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
    initialState p = { frontier: { queue: newQ { trail: N.singleton p.start, cost }
                                 , set: S.singleton p.start 
                                 }
                     , explored: S.empty
                     }
                     where cost = p.heuristic p.start p.goal
                           newQ = Q.singleton cost <<< Path
    eval p a = void $ evalRWST a p $ initialState p
    whileM' = flip whileM_ (pure unit)
    step' emit = lift2 (<*) (pure <<< isRight) (liftAff <<< emit) =<< step 


