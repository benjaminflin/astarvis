module Event.MouseEvent (ClickEvent, fromEvent, toEvent, clientCoordinates) where

import Prelude
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..), type_)

foreign import data ClickEvent :: Type

fromEvent :: Event -> Maybe ClickEvent
fromEvent event = case type_ event of
  EventType "click" -> Just $ unsafeCoerce event
  EventType "mousedown" -> Just $ unsafeCoerce event
  EventType "mousemove" -> Just $ unsafeCoerce event
  EventType "mouseup" -> Just $ unsafeCoerce event
  _ -> Nothing

toEvent :: ClickEvent -> Event
toEvent = unsafeCoerce

type Coordinates
  = { x :: Number, y :: Number }

foreign import clientX :: ClickEvent -> Number

foreign import clientY :: ClickEvent -> Number

clientCoordinates :: ClickEvent -> Coordinates
clientCoordinates event = { x: clientX event, y: clientY event }
