{-# LANGUAGE ExistentialQuantification #-}

module ExpressionProblem3b
(
    Perimeter(..)
  , Rectangle(..)
  , ShapeV2(..)
)
where

import ExpressionProblem3a

-- Extension to API
-- New shape: rectangle
-- New operation: perimeter

data Rectangle = Rectangle Double Double
               deriving Show
instance Area Rectangle where
  area (Rectangle width height) = width * height
instance Perimeter Rectangle where
  perimeter (Rectangle width height) = 2.0 * width + 2.0 * height

class Perimeter s where
  perimeter :: s -> Double
instance Perimeter Square where
  perimeter (Square side) = 4.0 * side
instance Perimeter Circle where
  perimeter (Circle radius) = 2.0 * pi * radius

data ShapeV2 = forall a. (Area a, Perimeter a) => ShapeV2 a
instance Area ShapeV2 where
  area (ShapeV2 s) = area s
instance Perimeter ShapeV2 where
  perimeter (ShapeV2 s) = perimeter s
