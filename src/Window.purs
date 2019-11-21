module Window.DevicePixelRatio where

import Web.HTML.Window (Window(..))
import Effect

foreign import devicePixelRatio :: Window -> Effect Number
