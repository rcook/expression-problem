module ExpressionProblem2b
(
    Rectangle(..)
  , perimeter
)
where

import ExpressionProblem2a

-- Extension to API
-- New shape: rectangle
-- New operation: perimeter

data Rectangle = Rectangle Double Double
               deriving Show
instance Area Rectangle where
  area (Rectangle width height) = width * height

class Perimeter s where
  perimeter :: s -> Double
instance Perimeter Square where
  perimeter (Square side) = 4.0 * side
instance Perimeter Circle where
  perimeter (Circle radius) = 2.0 * pi * radius
instance Perimeter Rectangle where
  perimeter (Rectangle width height) = 2.0 * width + 2.0 * height
