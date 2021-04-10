module Keys (onKeyUp, onKeyDown) where

import Effect
import Prelude
import Data.Maybe
import Data.Set (Set, delete, insert)
import Effect.Ref (Ref, read, write)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Partial.Unsafe (unsafePartial)

import State

keyToDir "w" = Just UP
keyToDir "ArrowUp" = Just UP
keyToDir "s" = Just DOWN
keyToDir "ArrowDown" = Just DOWN
keyToDir "a" = Just LEFT
keyToDir "ArrowLeft" = Just LEFT
keyToDir "d" = Just RIGHT
keyToDir "ArrowRight" = Just RIGHT
keyToDir _ = Nothing

keyListener :: (Direction -> Set Direction -> Set Direction) -> Ref (Set Direction) -> Event -> Effect Unit
keyListener op ref event = do 
  keys <- read ref
  let direction = keyToDir $ key $ unsafePartial $ fromJust $ fromEvent $ event
  case direction of
    Just dir -> write (op dir keys) ref
    Nothing -> mempty


onKeyUp = keyListener delete
onKeyDown = keyListener insert
