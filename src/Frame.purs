module Frame (step) where

import Effect
import Prelude
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Foldable
import Effect.Now (now)
import Effect.Ref (Ref, read, write)
import Data.Set (Set, delete, insert)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame )

import State
import Rendering
import Utils

transformState ::  GameState -> Set Direction -> Milliseconds -> Instant -> GameState
transformState state keys (Milliseconds delta) time = state { player = adjustTimings (foldl applyKeys standingPlayer keys) }
  where
  standingPlayer = state.player { animation = STANDING }
  applyKeys obj UP = obj { y = obj.y - (obj.speed * delta) / 1000.0, direction = UP, animation = MOVEMENT }
  applyKeys obj DOWN = obj { y = obj.y + (obj.speed * delta) / 1000.0, direction = DOWN, animation = MOVEMENT }
  applyKeys obj LEFT = obj { x = obj.x - (obj.speed * delta) / 1000.0, direction = LEFT, animation = MOVEMENT }
  applyKeys obj RIGHT = obj { x = obj.x + (obj.speed * delta) / 1000.0, direction = RIGHT, animation = MOVEMENT }
  adjustTimings obj = case state.player.animation of
    STANDING -> obj { frameStart = time}
    MOVEMENT -> obj

step :: Instant -> GameState -> Ref (Set Direction) -> Effect Unit
step lastFrame state keysRef = do
  time <- now
  keys <- read keysRef
  let delta = substractMilliseconds (unInstant time) (unInstant lastFrame)
  let newState = transformState state keys delta time

  render newState
  currentWindow <- window
  void $ requestAnimationFrame (step time newState keysRef) currentWindow

