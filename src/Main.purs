module Main where

import Prelude
import Math (sqrt)
import Data.Ord (abs)
import Data.Int (toNumber)
import Data.Set as S
import Effect.Aff (launchAff_)
import Data.AffStream (consume, (<?>), scan, take)
import AStar (Tile(..), Params, astarS)
import UI (actionS, Action(..)) 
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)

main :: Effect Unit
main = launchAff_ $ consume (liftEffect <<< log <<< show) $ astarS (pure initialParams) 
    where
    isEdit (Draw _) = true
    isEdit (Erase _) = true
    isEdit _ = false

    initialParams
        = { map: S.fromFoldable [ Tile 0 1
                                , Tile 1 1
                                , Tile 2 1 
                                , Tile 3 1
                                , Tile 4 1
                                ]
          , start: Tile 0 0
          , goal: Tile 2 2
          , heuristic: (\(Tile x y) (Tile z w) -> sqrt $ toNumber $ (x-z)*(x-z) + (y-w)*(y-w))
          } 
    
