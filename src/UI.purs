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

type Elements
  = { playButton :: Element
    , drawButton :: Element
    , moveButton :: Element
    , resetButton :: Element
    , canvas :: Element
    }

type Listeners
  = { playListener :: EventListener
    , drawListener :: EventListener
    , moveListener :: EventListener
    , resetListener :: EventListener
    , canvasListener :: EventListener
    }

data Emitted
  = Play Element Event
  | Draw Element Event
  | Move Element Event
  | Reset Element Event
  | Canvas Element Event

getElement :: String -> MaybeT Effect Element
getElement id =
  MaybeT do
    windowHTML <- window
    documentHTML <- document windowHTML
    getElementById id $ toNonElementParentNode documentHTML

getButtons :: MaybeT Effect Elements
getButtons = do
  playButton <- getElement "play"
  drawButton <- getElement "draw"
  moveButton <- getElement "move"
  resetButton <- getElement "reset"
  canvas <- getElement "astar-vis"
  pure
    $ { playButton
      , drawButton
      , moveButton
      , resetButton
      , canvas
      }

makeListeners :: Elements -> Emitter Effect Emitted Unit -> Effect Listeners
makeListeners { playButton, drawButton, moveButton, resetButton, canvas } emitter = do
  playListener <- makeListener $ Play playButton
  drawListener <- makeListener $ Draw drawButton
  moveListener <- makeListener $ Move moveButton
  resetListener <- makeListener $ Reset resetButton
  canvasListener <- makeListener $ Canvas canvas
  pure
    $ { playListener
      , drawListener
      , moveListener
      , resetListener
      , canvasListener
      }
  where
  makeListener f = eventListener $ emit emitter <<< f

attachListeners :: Elements -> Listeners -> Emitter Effect Emitted Unit -> Effect Unit
attachListeners { playButton, drawButton, moveButton, resetButton, canvas } { playListener, drawListener, moveListener, resetListener, canvasListener } emitter = do
  addListener (EventType "click") playListener playButton
  addListener (EventType "click") drawListener drawButton
  addListener (EventType "click") moveListener moveButton
  addListener (EventType "click") resetListener resetButton
  addListener (EventType "click") canvasListener canvas
  where
  addListener eventType listener el = addEventListener eventType listener false (toEventTarget el)

producer :: forall m. MonadAff m => Producer Emitted m Unit
producer =
  produce' \emitter ->
    runMaybeT_ do
      btns <- getButtons
      listeners <- lift $ makeListeners btns emitter
      lift $ attachListeners btns listeners emitter
  where
  runMaybeT_ = void <<< runMaybeT

makeConsumer :: forall m. MonadAff m => (Emitted -> m Unit) -> Consumer Emitted m Unit
makeConsumer changeState = forever $ lift <<< changeState =<< await

startUI :: forall s. Consumer Emitted (StateT s Aff) Unit -> s -> Effect Unit
startUI consumer = launchAff_ <<< (runStateT $ runProcess $ consumer `pullFrom` producer)
