module Rendering (render, loadResources) where

import Effect
import Prelude
import Data.Maybe
import Graphics.Canvas
import Partial.Unsafe (unsafePartial)
import Effect.Now (now)
import Data.Time.Duration
import Data.Int (floor, toNumber)
import Data.DateTime.Instant (unInstant)
import Effect.Console (log, error)

import State
import Utils

width = 800.0
height = 600.0

render :: GameState -> Effect Unit
render state = do
  maybeCanvas <- getCanvasElementById "output"
  let canvas = unsafePartial $ fromJust maybeCanvas
  setCanvasWidth canvas width
  setCanvasHeight canvas height
  context <- getContext2D $ canvas
  setFillStyle context "white"
  fillRect context { x: 0.0, y: 0.0, width: width, height: height }
  draw context state.player


loadResources = tryLoadImage "ludzik.png"


draw :: Context2D -> GameObject -> Effect Unit
draw context obj = do
  time <- now
  drawImageFull context obj.image (getX obj.animation $ substractMilliseconds (unInstant time) (unInstant obj.frameStart)) (getY obj.direction * obj.height) obj.width obj.height obj.x obj.y obj.width obj.height
  where
    getY UP = 3.0
    getY DOWN = 0.0
    getY LEFT = 1.0
    getY RIGHT = 2.0
    getX STANDING _ = 0.0
    getX MOVEMENT (Milliseconds delta) = toNumber (mod (floor $ delta / 100.0) 4) * obj.width
