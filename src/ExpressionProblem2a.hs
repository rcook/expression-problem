module ExpressionProblem2a
(
    Area(..)
  , Circle(..)
  , Square(..)
  , area
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
