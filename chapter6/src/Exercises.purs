module Exercises where

import Prelude
import Data.Foldable (foldMap, foldr, foldl, class Foldable)

-- instance showShape :: Show Shape where
--   showShape (Circle c r) =
--     "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
--   showShape (Rectangle c w h) =
--     "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
--   showShape (Line start end) =
--     "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
--   showShape (Text loc text) =
--     "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"

-- Complex numbers.
newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex x) = "(" <> show x.real <> " + " <> show x.imaginary <> "i)"

instance eqComplex :: Eq Complex where
  eq (Complex x) (Complex y) = x.real == y.real && x.imaginary == y.imaginary

-- NonEmpty arrays.
data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show ([x] <> xs)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f acc ([x] <> xs)
  foldr f acc (NonEmpty x xs) = foldr f acc ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)
