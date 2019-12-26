module UI where

import Prelude (Unit, bind, void, ($), (<$>), (<<<))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce', emit, Emitter)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.List (List(..), zip, (:))
import Data.Tuple (Tuple(..), uncurry)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM as DOM
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.Event.Event (Event, EventType(..))
import Record.Extra (mapRecord, sequenceRecord, zipRecord)
import Data.Traversable (sequence)

type ElementMap a
  = { play :: a
    , draw :: a
    , erase :: a
    , move :: a
    , reset :: a
    , canvas :: a
    }

type Elements
  = ElementMap DOM.Element

data Emitted
  = Play Elements Event
  | Draw Elements Event
  | Erase Elements Event
  | Move Elements Event
  | Reset Elements Event
  | MouseMove Elements Event

fromId :: String -> Effect (Maybe DOM.Element)
fromId id = do
  windowHTML <- window
  documentHTML <- document windowHTML
  getElementById id $ toNonElementParentNode documentHTML

fromIds :: ElementMap String -> MaybeT Effect Elements
fromIds ids = sequenceRecord $ mapRecord (MaybeT <<< fromId) ids

makeListeners :: ElementMap String -> Elements -> Emitter Effect Emitted Unit -> Effect (ElementMap (List EventListener))
makeListeners ids domElementMap emitter = sequenceRecord $ mapRecord toListeners ids
  where
  toListeners id = sequence $ listen <<< (_ $ domElementMap) <$> (toConstructorList id)

  toConstructorList id = case id of
    "play" -> (Play : Nil)
    "draw" -> (Draw : Nil)
    "erase" -> (Erase : Nil)
    "move" -> (Move : Nil)
    "reset" -> (Reset : Nil)
    "astar-vis" -> (MouseMove : Nil)
    _ -> Nil

  listen = eventListener <<< (emit emitter <<< _)

attachListeners :: ElementMap ({ listeners :: List EventListener, id :: String, domElement :: DOM.Element }) -> Effect Unit
attachListeners elements = void $ sequenceRecord $ mapRecord attachAll elements
  where
  attachAll { domElement, listeners, id } = sequence $ uncurry (attach domElement) <$> (zip listeners $ toEventTypes id)

  attach domElement listener eventType = addEventListener eventType listener false (toEventTarget domElement)

  toEventTypes id = case id of
    "astar-vis" -> EventType "mousemove" : Nil
    _ -> EventType "click" : Nil

producer :: forall m. MonadAff m => Producer Emitted m Unit
producer =
  produce' \emitter ->
    runMaybeT_ do
      domElementMap <- fromIds ids
      listeners <- lift $ makeListeners ids domElementMap emitter
      let
        zipped = zipRecordWith createRecord listeners $ zipRecord ids domElementMap
      lift $ attachListeners zipped
  where
  runMaybeT_ = void <<< runMaybeT

  createRecord listeners (Tuple id domElement) = { listeners, id, domElement }

  zipRecordWith fn r1 r2 = mapRecord (uncurry fn) $ zipRecord r1 r2

  ids =
    { play: "play"
    , draw: "draw"
    , erase: "erase"
    , move: "move"
    , reset: "reset"
    , canvas: "astar-vis"
    }
