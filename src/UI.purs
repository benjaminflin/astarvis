module UI where

import Prelude (class Show, bind, const, discard, pure, show, ($), (<$), (<$>), (<<<), (<>), (=<<), (==))

import Data.AffStream
import Data.AffStream as S

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Array (head)
import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)

import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.HTML (window)
import Web.UIEvent.MouseEvent (fromEvent, clientX, clientY, buttons)


data Tool 
    = Move
    | Pen
    | Eraser

data Coords = Coords Int Int

data Event'
    = SetTool Tool
    | MouseMove Coords

data Action 
    = Pan Coords 
    | Draw Coords 
    | Erase Coords 
    | Play
    | Pause
    | Reset

instance showCoords :: Show Coords where
    show (Coords i j) = "(" <> show i <> ", " <> show j <> ")"

instance showAction :: Show Action where
    show (Pan c) = "Pan: " <> show c 
    show (Draw c) = "Draw: " <> show c 
    show (Erase c) = "Erase: " <> show c 
    show (Play) = "Play"
    show (Pause) = "Pause"
    show (Reset) = "Reset"


scan :: forall a b. (b -> a -> b) -> b -> Stream a -> Stream b
scan f x0 s = fromCallback $ \emit -> tailRecM go { x: x0, emit }
    where
    go { x, emit } = do
        a <- fromJust' <<< head <$> S.take 1 s 
        let b = f x a
        emit b
        pure $ Loop { x: b, emit }

fromJust' :: forall a. Maybe a -> a
fromJust' a = unsafePartial $ fromJust a

elementById :: String -> Effect (Maybe Element)
elementById id = 
    (getElementById id) 
    <<< toNonElementParentNode 
    <<< toDocument 
    =<< document 
    =<< window 

clientCoords :: Event -> Coords
clientCoords = mkCoords <<< fromJust' <<< fromEvent
    where
    mkCoords e = Coords (clientX e) (clientY e)

eventS :: String -> String -> Stream Event
eventS id evt = fromCallback $ \emit -> liftEffect $ do
    l <- eventListener $ launchAff_ <<< emit 
    t <- toEventTarget <<< fromJust' <$> elementById id 
    addEventListener (EventType evt) l false t

toolS :: Stream Tool 
toolS = 
    (Move <$ eventS "move" "click") 
    <> (Pen <$ eventS "draw" "click")
    <> (Eraser <$ eventS "erase" "click")

canvasS :: Stream Coords
canvasS = clientCoords <$> (f <?> eventS "astar-vis" "mousemove")
    where f = (_ == 1) <<< buttons <<< fromJust' <<< fromEvent

μ :: Stream Action
μ = toolS >>- ((_ <$> canvasS) <<< tool2Action)
    where 
    tool2Action (Move) = Pan 
    tool2Action (Pen) = Draw 
    tool2Action (Eraser) = Erase 

playS :: Stream Action
playS = scan (const <<< toggle) Play (eventS "play" "click")
    where toggle Play = Pause
          toggle Pause = Play
          toggle _ = Pause

resetS :: Stream Action
resetS = Reset <$ (eventS "reset" "click")


actionS :: Stream Action
actionS = μ <> playS <> resetS
