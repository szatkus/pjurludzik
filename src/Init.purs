module Init (start) where

import Effect
import Prelude
import Web.HTML (window)
import Effect.Console (log, error)
import Graphics.Canvas (CanvasImageSource)
import Data.Maybe
import Effect.Now (now)
import Data.DateTime.Instant (Instant)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.KeyboardEvent.EventTypes
import Web.HTML.Window (requestAnimationFrame, toEventTarget)
import Effect.Ref as Ref
import Data.Set as Set

import Keys
import Frame
import State

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
