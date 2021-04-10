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
import Effect.Now (now)
import Data.DateTime.Instant (Instant)


data Direction = UP | DOWN | LEFT | RIGHT

type GameObject = { x :: Int, y :: Int, speed :: Int, image :: CanvasImageSource, frameStart :: Instant, direction :: Direction}
type GameState = { player :: GameObject }

step :: Effect Unit
step = do
  log "dupa"

start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  void $ requestAnimationFrame step currentWindow
  where state = { player : { x: 50, y: 50, speed: 10, image: image, frameStart: now, direction: DOWN }}

start Nothing = error "Się wyjebało"

main :: Effect Unit
main = tryLoadImage "ludzik.png" start
