module Utils (substractMilliseconds) where

import Prelude
import Data.Time.Duration

substractMilliseconds (Milliseconds a) (Milliseconds b) = Milliseconds (a - b)
