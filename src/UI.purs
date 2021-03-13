module UI where

import Prelude
import Control.Alt ((<|>))
import Data.AffStream
import Data.Vec (Vec, vec2)
import Data.Maybe (Maybe, fromJust)
import Data.Typelevel.Num (D2)
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

data Action 
    = Pan (Vec D2 Int)
    | Draw (Vec D2 Int)
    | Erase (Vec D2 Int) 
    | Play
    | Pause
    | Reset

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
    show (Pan c) = "Pan: " <> show c 
    show (Draw c) = "Draw: " <> show c 
    show (Erase c) = "Erase: " <> show c 
    show (Play) = "Play"
    show (Pause) = "Pause"
    show (Reset) = "Reset"


fromJust' :: forall a. Maybe a -> a
fromJust' a = unsafePartial $ fromJust a

elementById :: String -> Effect (Maybe Element)
elementById id = 
    (getElementById id) 
    <<< toNonElementParentNode 
    <<< toDocument 
    =<< document 
    =<< window 

clientCoords :: Event -> Vec D2 Int
clientCoords = mkCoords <<< fromJust' <<< fromEvent
    where
    mkCoords e = vec2 (clientX e) (clientY e)

eventS :: String -> String -> Stream Event
eventS id evt = fromCallback $ \emit -> liftEffect $ do
    l <- eventListener $ launchAff_ <<< emit 
    t <- toEventTarget <<< fromJust' <$> elementById id 
    addEventListener (EventType evt) l false t


toolS :: Stream Tool 
toolS = 
    (Move <$ eventS "move" "click") 
    <|> (Pen <$ eventS "draw" "click")
    <|> (Eraser <$ eventS "erase" "click")

mouseMoveS :: Stream (Vec D2 Int)
mouseMoveS = clientCoords <$> (f <?> eventS "astar-vis" "mousemove")
    where f = (_ == 1) <<< buttons <<< fromJust' <<< fromEvent

data MouseClick 
    = MouseDown (Vec D2 Int)
    | MouseUp (Vec D2 Int)

mouseDownS :: Stream MouseClick
mouseDownS = MouseDown <<< clientCoords <$> eventS "astar-vis" "mousedown" 

mouseUpS :: Stream MouseClick
mouseUpS = MouseUp <<< clientCoords <$> eventS "astar-vis" "mouseup" 

panS :: Stream (Vec D2 Int)
panS = (mouseDownS <|> mouseUpS) >>- f 
    where f (MouseDown s) 
              = (s - _) <$> mouseMoveS 
          f _ = empty 

toolS' :: Stream Action
toolS' = toolS >>- tool2Action
    where 
    tool2Action (Move) = Pan <$> panS
    tool2Action (Pen) = Draw <$> mouseMoveS 
    tool2Action (Eraser) = Erase <$> mouseMoveS 

playS :: Stream Action
playS = scan (const <<< toggle) Pause (eventS "play" "click")
    where toggle Play = Pause
          toggle Pause = Play
          toggle _ = Pause

resetS :: Stream Action
resetS = Reset <$ (eventS "reset" "click")

actionS :: Stream Action
actionS = toolS' <|> playS <|> resetS
