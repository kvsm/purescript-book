module Exercises where

import Prelude

-- Not stack safe (crashes with large inputs).
factorial' :: Int -> Int
factorial' 0 = 0
factorial' n = n + factorial (n - 1)

-- Stack safe due to tail call optimisation.
-- (recursive call is in tail position ie. it is the final expression to be evaluated)
factorial :: Int -> Int
factorial n = fact' n 0 where
  fact' 0 acc = acc
  fact' n acc = fact' (n - 1) (acc + n)

-- This is not stack safe - due to double recursion tail call optimisation
-- is not possible. Although the exercise asks us to implement this algorithm
-- specifically, an entirely different approach would be required to make this
-- function safe.
binomial :: Int -> Int -> Int
binomial 0 n = 1
binomial k 0 = 0
binomial k n  | k == n    = 1
              | otherwise = (binomial (k - 1) (n - 1)) + (binomial k (n - 1))
