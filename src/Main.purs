module Main where

import Effect
import Prelude

import Init
import Rendering

main :: Effect Unit
main = loadResources start
