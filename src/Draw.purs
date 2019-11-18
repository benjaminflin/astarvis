module Draw where

import Prelude
import Graphics.Canvas
import Effect
import Data.Maybe
import Control.Monad.Except.Trans
import Data.Either
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight, Window(..))
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)

data Error
  = CanvasNotFoundError

instance showErr :: Show Error where
  show CanvasNotFoundError = "CanvasNotFoundError"

getCanvas :: ExceptT Error Effect CanvasElement
getCanvas =
  ExceptT
    $ do
        canvas <- getCanvasElementById "astar-vis"
        case canvas of
          Just canvas -> return canvas
          Nothing -> throw CanvasNotFoundError
  where
  throw = pure <<< Left

  return = pure <<< Right

getWindowDimensions :: Effect (Dimensions)
getWindowDimensions = do
  window <- window
  width <- innerWidth window
  height <- innerHeight window
  pure ({ width: toNumber width, height: toNumber height })

drawAStar :: Effect (Either Error Unit)
drawAStar =
  runExceptT
    $ do
        canvas <- getCanvas
        dimensions <- lift $ getWindowDimensions
        lift $ setCanvasDimensions canvas dimensions
        ctx <- lift $ getContext2D canvas
        lift $ strokePath ctx
          $ do
              moveTo ctx 10.5 10.5
              lineTo ctx 200.5 200.5
              lineTo ctx 10.5 200.5
              closePath ctx
