module ExpressionProblem1b
(
    perimeter
)
where

import ExpressionProblem1a

-- Extension to API
-- New operation: perimeter
-- Cannot define new shape since Shape is closed

perimeter :: Shape -> Double
perimeter s =
  case s of
       Square side -> 4.0 * side
       Circle radius -> 2 * pi * radius
