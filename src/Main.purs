module Main where

import Prelude
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Data.AffStream (consume)
import UI (actionS) 
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = launchAff_ $ consume (liftEffect <<< log <<< show) actionS  
