module UI where

import Prelude (class Show, bind, const, show, ($), (<$), (<$>), (<<<), (<>), (=<<), (==), (-))
import Control.Alt ((<|>))
import Data.AffStream
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
    <|> (Pen <$ eventS "draw" "click")
    <|> (Eraser <$ eventS "erase" "click")

mouseMoveS :: Stream Coords
mouseMoveS = clientCoords <$> (f <?> eventS "astar-vis" "mousemove")
    where f = (_ == 1) <<< buttons <<< fromJust' <<< fromEvent

data MouseClick 
    = MouseDown Coords
    | MouseUp Coords

mouseDownS :: Stream MouseClick
mouseDownS = MouseDown <<< clientCoords <$> eventS "astar-vis" "mousedown" 

mouseUpS :: Stream MouseClick
mouseUpS = MouseUp <<< clientCoords <$> eventS "astar-vis" "mouseup" 

panS :: Stream Coords
panS = (mouseDownS <|> mouseUpS) >>- f 
    where f (MouseDown (Coords x y)) 
              = (\(Coords z w) -> Coords (x - z) (y - w)) <$> mouseMoveS 
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
