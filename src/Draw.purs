module Draw where

import Prelude

import Control.Apply (lift2)
import Control.Monad.RWS (RWST, evalRWST, ask, get, gets, modify_)
import Data.AffStream (Stream, subscribe)
import Data.Either (Either(..))
import Data.HashSet as S
import Data.HashSet (HashSet) 
import Data.Traversable (sequence_)
import Data.Foldable (class Foldable)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, vec2, (!!))
import Web.DOM (Element)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Graphics.Canvas
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import UI (fromJust')
import AStar (Tile(..), Result, State, Params)
import Data.Tuple.Nested
import Data.Tuple (fst, snd)




data Command
    = Move' (Vec D2 Int)
    | Play'
    | Pause'
    | Draw' (Params /\ (Either Result State))

type Set = HashSet

canvas :: Effect CanvasElement 
canvas = fromJust' <$> getCanvasElementById "astar-vis"

context :: Effect Context2D
context = getContext2D =<< canvas

type DrawState
    = { position :: Vec D2 Int }

type Draw = RWST Context2D Unit DrawState Effect

viewScale :: Int
viewScale = 40

toTileCoords :: Vec D2 Int -> Effect (Vec D2 Int)
toTileCoords v = do
    d <- dimensions
    let width = d !! d0
        height = d !! d1 
        x = v !! d0
        y = v !! d1
    pure $ vec2 ((x - (width `div` 2)) `div` viewScale) ((y - (height `div` 2)) `div` -viewScale - 1)
    

runDraw :: forall a. Context2D -> Draw a -> Effect a
runDraw ctx d = fst <$> evalRWST d ctx { position : vec2 0 0 }   

gfx :: forall a. (Context2D -> a -> Effect Unit) -> a -> Draw Unit
gfx f a = (liftEffect <<< flip f a) =<< ask 

dimensions :: Effect (Vec D2 Int)
dimensions = do  
    w <- window
    width <- innerWidth w
    height <- innerHeight w
    pure $ vec2 width height 


drawTile :: Tile -> Draw Unit
drawTile (Tile v) = gfx fillRect { x: toNumber $ v !! d0
                                 , y: toNumber $ v !! d1
                                 , width: 1.0
                                 , height: 1.0
                                 }

drawTiles :: forall f. Foldable f => Functor f => f Tile -> Draw Unit
drawTiles = sequence_ <<< map drawTile

drawExplored :: Set Tile -> Draw Unit
drawExplored s = do
    gfx setFillStyle "#f3c13a"
    drawTiles <<< S.toArray $ s

drawPath :: Result -> Draw Unit
drawPath r = do
    gfx setFillStyle "#1f4788"
    runMaybe 
        $ drawTiles
        <<< (_.trail) 
        <<< unwrap <$> r
    where runMaybe = fromMaybe (pure unit)

drawEndpoints :: Tile -> Tile -> Draw Unit
drawEndpoints s e = do
    gfx setFillStyle "#eb4034"
    drawTile s
    gfx setFillStyle "#26A65B"
    drawTile e

drawMap :: Set Tile -> Draw Unit
drawMap s = do
    gfx setFillStyle "#757d75"
    drawTiles <<< S.toArray $ s 

initTransform :: Draw Unit
initTransform = do
    v <- liftEffect dimensions
    let width = toNumber $ v !! d0 
        height = toNumber $ v !! d1 
    liftEffect $ flip setCanvasDimensions { width, height } =<< canvas
    gfx translate { translateX: width / 2.0, translateY: height / 2.0 }
    gfx scale { scaleX: toNumber viewScale, scaleY: toNumber $ -viewScale }



draw :: Stream Command -> Effect Unit
draw comS = do
    ctx <- context
    runDraw ctx initTransform 
    subscribe' comS ctx draw'
    where
    -- draw' (Move' v) = do
    draw' (Draw' (p /\ r)) = do
        drawParams p
        drawRes r
    draw' _ = pure unit 
    drawRes (Left r) = drawPath r
    drawRes (Right ({ explored })) = drawExplored explored
    drawParams { map, start, goal } = do
       drawMap map
       drawEndpoints start goal
    subscribe' :: forall a. Stream a -> Context2D -> (a -> Draw Unit) -> Effect Unit
    subscribe' s ctx f = launchAff_ $ flip subscribe s $ liftEffect <<< runDraw ctx <<< f 
