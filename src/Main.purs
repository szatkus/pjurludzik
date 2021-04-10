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
import Data.Set as Set
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)


data Direction = UP | DOWN | LEFT | RIGHT

type GameObject = { x :: Number, y :: Number, width :: Number, height :: Number, speed :: Number, image :: CanvasImageSource, frameStart :: Instant, direction :: Direction}
type GameState = { player :: GameObject }


draw :: GameObject -> Effect Unit
draw obj = do
  canvas <- getCanvasElementById "output"
  context <- getContext2D $ unsafePartial $ fromJust canvas
  drawImageFull context obj.image 0.0 0.0 obj.width obj.height obj.x obj.y obj.width obj.height
  where frameCount = 4


step :: GameState -> Ref.Ref (Set.Set String) -> Effect Unit
step state keys = do
  draw state.player
  currentWindow <- window
  void $ requestAnimationFrame (step state keys) currentWindow


start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  keys <- Ref.new Set.empty
  time <- now
  let state = { player : { x: 50.0, y: 50.0, width: 34.0, height: 52.0, speed: 10.0, image: image, frameStart: time, direction: DOWN }}
  void $ requestAnimationFrame (step state keys) currentWindow

start Nothing = error "Się wyjebało"


main :: Effect Unit
main = tryLoadImage "ludzik.png" start
