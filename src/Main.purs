module Main where

import Prelude
import Math (sqrt)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Int (toNumber, floor)
import Data.Tuple.Nested
import Data.HashSet as S
import Effect.Aff (launchAff_)
import Data.AffStream (Stream, consume, scan, filter, flatten)
import Data.Either (Either)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2, dotProduct)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)

import AStar (Tile(..), astarS, Params, State, Result)
import UI (actionS, Action(..)) 
import Draw (Command(..), draw, toTileCoords)

import Debug.Trace 


main :: Effect Unit
main = draw eventS 


eventS :: Stream Command
eventS = actionS' <|> astarS'
    where
    astarS' = Draw' <$> (astarS paramsS)
    actionS' = actionS >>= case _ of
        Pan c -> pure $ Move' c
        Play -> pure $ Play'
        Pause -> pure $ Pause'
        _ -> empty 

paramsS :: Stream Params
paramsS = liftEffect =<< scan f (pure initialParams) actionS
    where
    f lastParams (Draw v) = do
        params <- lastParams
        v'     <- toTileCoords v 
        pure $ params { map = S.insert (Tile v') params.map }
    f lastParams (Erase v) = do
       params <- lastParams
       v'     <- toTileCoords v 
       pure $ params { map = S.delete (Tile v') params.map }
    f lastParams _ = lastParams 

initialParams :: Params
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
