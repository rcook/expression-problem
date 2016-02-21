module ExpressionProblem1a
(
    Shape(..)
  , area
)
where

-- Initial API
-- Two shapes: square and circle
-- One operation: area

data Shape = Square Double | Circle Double
           deriving Show

area :: Shape -> Double
area s =
  case s of
       Square side -> side * side
       Circle radius -> pi * radius * radius
