module Main where

import Prelude
import Effect.Aff (launchAff_)
import Data.AffStream (consume)
import UI (actionS) 
import Effect (Effect)
import Debug.Trace (traceM)

main :: Effect Unit
main = launchAff_ $ consume traceM actionS  
