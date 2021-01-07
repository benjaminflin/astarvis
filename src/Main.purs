module Main where

import Prelude
import Math (sqrt)
import Data.Int (toNumber)
import Data.HashSet as S
import Effect.Aff (launchAff_)
import Data.AffStream (consume, scan)
import AStar (Tile(..), astarS)
import Data.Vec (vec2, dotProduct)
import UI (actionS, Action(..)) 
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)

main :: Effect Unit
main = launchAff_ $ consume (liftEffect <<< log <<< show) $ astarS $ scan f initialParams actionS
    where
    f a (Draw v) = a { map = S.insert (Tile v) a.map }
    f a (Erase v) = a { map = S.delete (Tile v) a.map }
    f a _ = a
    initialParams
        = { map: S.fromFoldable $ Tile <$> 
                                [ vec2 0 1
                                , vec2 1 1
                                , vec2 2 1 
                                , vec2 3 1
                                , vec2 4 1
                                ]
          , start: Tile $ vec2 0 0
          , goal: Tile $ vec2 2 2
          , heuristic: \(Tile v) (Tile v') -> sqrt $ toNumber $ join dotProduct (v - v')
          } 
