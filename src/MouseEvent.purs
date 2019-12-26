module Event.MouseEvent where

import Prelude
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..), type_)

foreign import data MouseEvent :: Type

fromEvent :: Event -> Maybe MouseEvent
fromEvent event = case type_ event of
  EventType "click" -> Just $ unsafeCoerce event
  EventType "mousedown" -> Just $ unsafeCoerce event
  EventType "mousemove" -> Just $ unsafeCoerce event
  EventType "mouseup" -> Just $ unsafeCoerce event
  _ -> Nothing

toEvent :: MouseEvent -> Event
toEvent = unsafeCoerce

type Coordinates
  = { x :: Number, y :: Number }

foreign import clientX :: MouseEvent -> Number

foreign import clientY :: MouseEvent -> Number

foreign import movementX :: MouseEvent -> Number

foreign import movementY :: MouseEvent -> Number

foreign import buttons :: MouseEvent -> Int

clientCoordinates :: MouseEvent -> Coordinates
clientCoordinates event = { x: clientX event, y: clientY event }

movementCoordinates :: MouseEvent -> Coordinates
movementCoordinates event = { x: movementX event, y: movementY event }
