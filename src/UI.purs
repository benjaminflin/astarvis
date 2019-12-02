module UI where

import Prelude
import Control.Coroutine
import Control.Coroutine.Aff (produce', emit, Emitter)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Maybe.Trans
import Control.Monad.Rec.Class (forever)
import Control.Parallel.Class (class Parallel)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.List
import Data.Tuple
import Data.Maybe
import Effect
import Effect.Aff
import Effect.Aff.Class
import Effect.Class (liftEffect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM (Element)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (addEventListener, eventListener, EventTarget, EventListener)
import Web.Event.Event
import Debug.Trace

type Elements
  = { playButton :: Element
    , drawButton :: Element
    , eraseButton :: Element
    , moveButton :: Element
    , resetButton :: Element
    , canvas :: Element
    }

type Listeners
  = { playListener :: EventListener
    , drawListener :: EventListener
    , eraseListener :: EventListener
    , moveListener :: EventListener
    , resetListener :: EventListener
    , canvasMouseDownListener :: EventListener
    , canvasMouseMoveListener :: EventListener
    , canvasMouseUpListener :: EventListener
    }

data EventT
  = MouseDown
  | MouseMove
  | MouseUp

data Emitted
  = Play Elements Event
  | Draw Elements Event
  | Erase Elements Event
  | Move Elements Event
  | Reset Elements Event
  | Canvas Elements EventT Event

getElement :: String -> MaybeT Effect Element
getElement id =
  MaybeT do
    windowHTML <- window
    documentHTML <- document windowHTML
    getElementById id $ toNonElementParentNode documentHTML

getElements :: MaybeT Effect Elements
getElements = do
  playButton <- getElement "play"
  drawButton <- getElement "draw"
  eraseButton <- getElement "erase"
  moveButton <- getElement "move"
  resetButton <- getElement "reset"
  canvas <- getElement "astar-vis"
  pure
    $ { playButton
      , drawButton
      , eraseButton
      , moveButton
      , resetButton
      , canvas
      }

makeListeners :: Elements -> Emitter Effect Emitted Unit -> Effect Listeners
makeListeners els emitter = do
  playListener <- makeListener $ Play els
  drawListener <- makeListener $ Draw els
  eraseListener <- makeListener $ Erase els
  moveListener <- makeListener $ Move els
  resetListener <- makeListener $ Reset els
  canvasMouseDownListener <- makeListener $ Canvas els MouseDown
  canvasMouseMoveListener <- makeListener $ Canvas els MouseMove
  canvasMouseUpListener <- makeListener $ Canvas els MouseUp
  pure
    $ { playListener
      , drawListener
      , eraseListener
      , moveListener
      , resetListener
      , canvasMouseDownListener
      , canvasMouseMoveListener
      , canvasMouseUpListener
      }
  where
  makeListener f = eventListener $ emit emitter <<< f

attachListeners :: Elements -> Listeners -> Emitter Effect Emitted Unit -> Effect Unit
attachListeners { playButton, drawButton, eraseButton, moveButton, resetButton, canvas } { playListener, drawListener, eraseListener, moveListener, resetListener, canvasMouseDownListener, canvasMouseMoveListener, canvasMouseUpListener } emitter = do
  addListener (EventType "click") playListener playButton
  addListener (EventType "click") drawListener drawButton
  addListener (EventType "click") eraseListener eraseButton
  addListener (EventType "click") moveListener moveButton
  addListener (EventType "click") resetListener resetButton
  addListener (EventType "mousedown") canvasMouseDownListener canvas
  addListener (EventType "mousemove") canvasMouseMoveListener canvas
  addListener (EventType "mouseup") canvasMouseUpListener canvas
  where
  addListener eventType listener el = addEventListener eventType listener false (toEventTarget el)

producer :: forall m. MonadAff m => Producer Emitted m Unit
producer =
  produce' \emitter ->
    runMaybeT_ do
      els <- getElements
      listeners <- lift $ makeListeners els emitter
      lift $ attachListeners els listeners emitter
  where
  runMaybeT_ = void <<< runMaybeT

makeConsumer :: forall m. MonadAff m => (Emitted -> m Unit) -> Consumer Emitted m Unit
makeConsumer changeState = forever $ lift <<< changeState =<< await

startUI :: forall s. Consumer Emitted (StateT s Aff) Unit -> s -> Effect Unit
startUI consumer = launchAff_ <<< (runStateT $ runProcess $ consumer `pullFrom` producer)
