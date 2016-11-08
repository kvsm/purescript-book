module Exercises where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Data.Picture (Point(..), Shape(..))
import Math (pow, pi)

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

{-
  This is not stack safe - due to double recursion tail call optimisation
  is not possible. Although the exercise asks us to implement this algorithm
  specifically, an entirely different approach would be required to make this
  function safe.
-}
binomial :: Int -> Int -> Int
binomial 0 n = 1
binomial k 0 = 0
binomial k n  | k == n    = 1
              | otherwise = (binomial (k - 1) (n - 1)) + (binomial k (n - 1))

-- Pattern matching on records.
type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x }} { address: { city: y }}
  | x == y    = true
  | otherwise = false

-- Pattern matching on arrays.
fromSingleton :: forall a. a -> Array a -> a
fromSingleton d [x] = x
fromSingleton d _ = d

-- Algebraic Data Types.
origin :: Point
origin = Point { x: 0.0, y: 0.0 }

myCircle :: Shape
myCircle = Circle origin 10.0

scaleAndCentre :: Shape -> Shape
scaleAndCentre (Circle _ r) = Circle origin (r * 2.0)
scaleAndCentre (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
scaleAndCentre (Line (Point start) (Point end)) = Line transformStart transformEnd where
  xdiff           = start.x - end.x
  ydiff           = start.y - end.y
  transformStart  = Point { x: -xdiff, y: -ydiff }
  transformEnd    = Point { x: xdiff, y: ydiff }
scaleAndCentre (Text _ str) = Text origin str

extractString :: Shape -> Maybe String
extractString (Text _ str) = Just str
extractString _ = Nothing

-- Vector graphics library.
area :: Shape -> Number
area (Circle _ r) = pi * (pow r 2.0)
area (Rectangle _ w h) = w * h
area _ = 0.0
