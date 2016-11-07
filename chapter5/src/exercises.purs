module Exercises where

import Prelude

factorial :: Int -> Int
factorial n = fact' n 0 where
  fact' 0 acc = acc
  fact' n acc = fact' (n - 1) (acc + n)
