module Exercises where

import Prelude

factorial :: Int -> Int
factorial n = fact' n 0 where
  fact' 0 acc = acc
  fact' n acc = fact' (n - 1) (acc + n)

binomial :: Int -> Int -> Int
binomial 0 n = 0
binomial k 0 = 0
binomial k n  | k == n    = 1
              | otherwise = (binomial (k - 1) (n - 1)) + (binomial k (n - 1))
