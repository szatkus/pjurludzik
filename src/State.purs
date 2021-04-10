module State (Direction(..), Animation(..), GameObject, GameState) where

import Graphics.Canvas
import Prelude
import Data.DateTime.Instant (Instant)

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

data Animation = STANDING | MOVEMENT

instance animationShow :: Show Animation where
  show STANDING = "STANDING"
  show MOVEMENT = "MOVEMENT"

type GameObject = { x :: Number, y :: Number, width :: Number, height :: Number, speed :: Number, image :: CanvasImageSource, frameStart :: Instant, direction :: Direction, animation :: Animation}
type GameState = { player :: GameObject }
