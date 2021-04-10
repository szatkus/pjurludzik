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
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Data.Ordering
import Web.HTML.Window (toEventTarget)

data Direction = UP | DOWN | LEFT | RIGHT

dirToInt UP = 0
dirToInt DOWN = 1
dirToInt LEFT = 2
dirToInt RIGHT = 3

instance directionOrdering :: Ord Direction where
  compare a b = compare (dirToInt a) (dirToInt b)

instance directionEq :: Eq Direction where
  eq a b = eq (dirToInt a) (dirToInt b)

type GameObject = { x :: Number, y :: Number, width :: Number, height :: Number, speed :: Number, image :: CanvasImageSource, frameStart :: Instant, direction :: Direction}
type GameState = { player :: GameObject }


draw :: GameObject -> Effect Unit
draw obj = do
  canvas <- getCanvasElementById "output"
  context <- getContext2D $ unsafePartial $ fromJust canvas
  drawImageFull context obj.image 0.0 0.0 obj.width obj.height obj.x obj.y obj.width obj.height
  where frameCount = 4


keyToDir "w" = Just UP
keyToDir "ArrowUp" = Just UP
keyToDir "s" = Just DOWN
keyToDir "ArrowDown" = Just DOWN
keyToDir "a" = Just LEFT
keyToDir "ArrowLeft" = Just LEFT
keyToDir "d" = Just RIGHT
keyToDir "ArrowRight" = Just RIGHT
keyToDir _ = Nothing

keyListener :: (Direction -> Set.Set Direction -> Set.Set Direction) -> Ref.Ref (Set.Set Direction) -> Event -> Effect Unit
keyListener op ref event = do 
  keys <- Ref.read ref
  let direction = keyToDir $ key $ unsafePartial $ fromJust $ fromEvent $ event
  case direction of
    Just dir -> Ref.write (op dir keys) ref
    Nothing -> mempty


onKeyUp = keyListener Set.delete
onKeyDown = keyListener Set.insert

step :: Instant -> GameState -> Ref.Ref (Set.Set Direction) -> Effect Unit
step lastFrame state keys = do
  time <- now
  draw state.player
  currentWindow <- window
  void $ requestAnimationFrame (step time state keys) currentWindow


start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  keys <- Ref.new Set.empty
  time <- now
  keyDownListener <- eventListener $ onKeyDown keys
  let state = { player : { x: 50.0, y: 50.0, width: 34.0, height: 52.0, speed: 10.0, image: image, frameStart: time, direction: DOWN }}

  addEventListener keydown keyDownListener false (toEventTarget currentWindow)
  void $ requestAnimationFrame (step time state keys) currentWindow

start Nothing = error "Się wyjebało, a nie powinno"


main :: Effect Unit
main = tryLoadImage "ludzik.png" start
