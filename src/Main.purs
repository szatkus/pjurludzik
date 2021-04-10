module Main where

import Effect
import Prelude
import Web.HTML.Window (requestAnimationFrame )
import Control.Plus (empty)
import Effect.Console (log, error)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)
import Data.Maybe
import Web.HTML (window)
import Control.Comonad (extract)
import Web.UIEvent.KeyboardEvent.EventTypes
import Effect.Now (now)
import Data.DateTime.Instant (Instant)
import Data.Set as Set
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Web.Event.EventTarget (addEventListener, eventListener)
import Data.Ordering
import Web.HTML.Window (toEventTarget)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration
import Data.Int (toNumber)

import State
import Keys
import Rendering
import Utils
import Frame


start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  keys <- Ref.new Set.empty
  time <- now
  keyDownListener <- eventListener $ onKeyDown keys
  keyUpListener <- eventListener $ onKeyUp keys
  
  let state = { player : { x: 50.0, y: 50.0, width: 34.0, height: 52.0, speed: 100.0, image: image, frameStart: time, direction: DOWN, animation: STANDING }}

  addEventListener keydown keyDownListener false (toEventTarget currentWindow)
  addEventListener keyup keyUpListener false (toEventTarget currentWindow)
  void $ requestAnimationFrame (step time state keys) currentWindow

start Nothing = error "Się wyjebało, a nie powinno"


main :: Effect Unit
main = loadResources start
