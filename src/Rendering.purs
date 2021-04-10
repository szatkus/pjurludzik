module Rendering (render, loadResources) where

import Effect
import Prelude
import Data.Maybe
import Graphics.Canvas
import Partial.Unsafe (unsafePartial)

import State

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
  drawImageFull context obj.image 0.0 (getY obj.direction * obj.height) obj.width obj.height obj.x obj.y obj.width obj.height
  where
    getY UP = 3.0
    getY DOWN = 0.0
    getY LEFT = 1.0
    getY RIGHT = 2.0
