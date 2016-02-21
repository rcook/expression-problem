{-# LANGUAGE ExistentialQuantification #-}

module ExpressionProblem3a
(
    Area(..)
  , Circle(..)
  , Shape(..)
  , Square(..)
)
where

-- Initial API
-- Two shapes: square and circle
-- One operation: area

data Square = Square Double
            deriving Show

data Circle = Circle Double
            deriving Show

class Area s where
  area :: s -> Double
instance Area Square where
  area (Square side) = side * side
instance Area Circle where
  area (Circle radius) = pi * radius * radius

data Shape = forall a. Area a => Shape a
instance Area Shape where
  area (Shape s) = area s
