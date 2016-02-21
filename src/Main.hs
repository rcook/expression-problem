module Main where

import qualified ExpressionProblem1a as EP1a
import qualified ExpressionProblem1b as EP1b
import qualified ExpressionProblem2a as EP2a
import qualified ExpressionProblem2b as EP2b
import qualified ExpressionProblem3a as EP3a
import qualified ExpressionProblem3b as EP3b

-- References
-- http://c2.com/cgi/wiki?ExpressionProblem
-- http://koerbitz.me/posts/Solving-the-Expression-Problem-in-Haskell-and-Java.html
-- http://koerbitz.me/posts/Sum-Types-Visitors-and-the-Expression-Problem.html
-- https://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf
-- https://oleksandrmanzyuk.wordpress.com/2014/06/18/from-object-algebras-to-finally-tagless-interpreters-2/
-- https://www.andres-loeh.de/OpenDatatypes.pdf
-- http://wadler.blogspot.com/2008/02/data-types-la-carte.html

-- Expression problem (1)
-- Using single ADT
--
-- Can add new functions: just create a new function, e.g. "perimeter"
-- 
-- Can't add new shapes: "Shape" ADT and existing functions are closed
expressionProblem1 :: IO ()
expressionProblem1 = do
  putStrLn "ExpressionProblem1"
  let shapes = [EP1a.Square 10.0, EP1a.Circle 10.0]
  print $ map EP1a.area shapes
  print $ map EP1b.perimeter shapes

-- Expression problem (2)
-- Using type classes and separate ADT for each shape
--
-- Straightforward to add new functions: declare a new type class and
-- implement an instance for each shape
-- 
-- Straightforward to add new shapes: declare a new ADT and implement existing
-- type classes
--
-- Problem: Heterogeneous lists of shapes not possible
expressionProblem2 :: IO ()
expressionProblem2 = do
  putStrLn "ExpressionProblem2"
  let
    squares = [EP2a.Square 10.0, EP2a.Square 20.0]
    circles = [EP2a.Circle 10.0, EP2a.Circle 20.0]
    rectangles = [EP2b.Rectangle 10.0 20.0, EP2b.Rectangle 20.0 30.0]
  print $ map EP2a.area squares
  print $ map EP2b.perimeter squares
  print $ map EP2a.area circles
  print $ map EP2b.perimeter circles
  print $ map EP2a.area rectangles
  print $ map EP2b.perimeter rectangles

-- Expression problem (2)
-- Use existential quantification to solve the previous problem
expressionProblem3 :: IO ()
expressionProblem3 = do
  putStrLn "ExpressionProblem3"
  let shapes = [
        EP3b.Shape $ EP3a.Square 10.0,
        EP3b.Shape $ EP3a.Circle 10.0,
        EP3b.Shape $ EP3b.Rectangle 10.0 20.0
        ]
  print $ map EP3a.area shapes
  print $ map EP3b.perimeter shapes

main :: IO ()
main =
  expressionProblem1 >>
  expressionProblem2 >>
  expressionProblem3
