{-# LANGUAGE ExistentialQuantification #-}

module ExpressionProblem3b
(
    Perimeter(..)
  , Rectangle(..)
  , Shape(..)
)
where

import qualified ExpressionProblem3a as EP3a

-- Extension to API
-- New shape: rectangle
-- New operation: perimeter

data Rectangle = Rectangle Double Double
               deriving Show
instance EP3a.Area Rectangle where
  area (Rectangle width height) = width * height
instance Perimeter Rectangle where
  perimeter (Rectangle width height) = 2.0 * width + 2.0 * height

class Perimeter s where
  perimeter :: s -> Double
instance Perimeter EP3a.Square where
  perimeter (EP3a.Square side) = 4.0 * side
instance Perimeter EP3a.Circle where
  perimeter (EP3a.Circle radius) = 2.0 * pi * radius

data Shape = forall a. (EP3a.Area a, Perimeter a) => Shape a
instance EP3a.Area Shape where
  area (Shape s) = EP3a.area s
instance Perimeter Shape where
  perimeter (Shape s) = perimeter s
