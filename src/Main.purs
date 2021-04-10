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
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration
import Data.Foldable
import Data.Int (toNumber)

data Direction = UP | DOWN | LEFT | RIGHT

dirToInt UP = 0
dirToInt DOWN = 1
dirToInt LEFT = 2
dirToInt RIGHT = 3

instance directionOrdering :: Ord Direction where
  compare a b = compare (dirToInt a) (dirToInt b)

instance directionEq :: Eq Direction where
  eq a b = eq (dirToInt a) (dirToInt b)

instance directionShow :: Show Direction where
  show UP = "UP"
  show DOWN = "DOWN"
  show LEFT = "LEFT"
  show RIGHT = "RIGHT"

type GameObject = { x :: Number, y :: Number, width :: Number, height :: Number, speed :: Number, image :: CanvasImageSource, frameStart :: Instant, direction :: Direction}
type GameState = { player :: GameObject }


draw :: Context2D -> GameObject -> Effect Unit
draw context obj = do
  drawImageFull context obj.image 0.0 (getY obj.direction * obj.height) obj.width obj.height obj.x obj.y obj.width obj.height
  where
    getY UP = 3.0
    getY DOWN = 0.0
    getY LEFT = 1.0
    getY RIGHT = 2.0


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

width = 800.0
height = 600.0

transformState ::  GameState -> Set.Set Direction -> Milliseconds -> GameState
transformState state keys (Milliseconds delta) = state { player = (foldl applyKeys state.player keys) }
  where 
  applyKeys obj UP = obj { y = obj.y - (obj.speed * delta) / 1000.0, direction = UP }
  applyKeys obj DOWN = obj { y = obj.y + (obj.speed * delta) / 1000.0, direction = DOWN }
  applyKeys obj LEFT = obj { x = obj.x - (obj.speed * delta) / 1000.0, direction = LEFT }
  applyKeys obj RIGHT = obj { x = obj.x + (obj.speed * delta) / 1000.0, direction = RIGHT }

step :: Instant -> GameState -> Ref.Ref (Set.Set Direction) -> Effect Unit
step lastFrame state keysRef = do
  time <- now
  keys <- Ref.read keysRef
  let delta = subMiliseconds (unInstant time) (unInstant lastFrame)
  let newState = transformState state keys delta

  maybeCanvas <- getCanvasElementById "output"
  let canvas = unsafePartial $ fromJust maybeCanvas
  setCanvasWidth canvas width
  setCanvasHeight canvas height
  context <- getContext2D $ canvas
  setFillStyle context "white"
  fillRect context { x: 0.0, y: 0.0, width: width, height: height }
  draw context newState.player

  currentWindow <- window
  void $ requestAnimationFrame (step time newState keysRef) currentWindow
  where subMiliseconds (Milliseconds a) (Milliseconds b) = Milliseconds (a - b)


start :: Maybe CanvasImageSource -> Effect Unit

start (Just image) =  do
  currentWindow <- window
  keys <- Ref.new Set.empty
  time <- now
  keyDownListener <- eventListener $ onKeyDown keys
  keyUpListener <- eventListener $ onKeyUp keys
  
  let state = { player : { x: 50.0, y: 50.0, width: 34.0, height: 52.0, speed: 100.0, image: image, frameStart: time, direction: DOWN }}

  addEventListener keydown keyDownListener false (toEventTarget currentWindow)
  addEventListener keyup keyUpListener false (toEventTarget currentWindow)
  void $ requestAnimationFrame (step time state keys) currentWindow

start Nothing = error "Się wyjebało, a nie powinno"


main :: Effect Unit
main = tryLoadImage "ludzik.png" start
