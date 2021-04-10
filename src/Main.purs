module Main where

import Effect
import Prelude
import Web.HTML.Window (requestAnimationFrame )
import Control.Plus (empty)
import Effect.Console (log, error)
import Graphics.Canvas
import Data.Maybe
import Web.HTML (window)
import Control.Comonad (extract)
import Web.UIEvent.KeyboardEvent.EventTypes

step :: Effect Unit
step = do
  log "dupa"

start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  void $ requestAnimationFrame step currentWindow

start Nothing = error "Się wyjebało"

main :: Effect Unit
main = tryLoadImage "ludzik.png" start
