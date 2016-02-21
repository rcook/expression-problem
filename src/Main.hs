{-# LANGUAGE ExistentialQuantification #-}

module Main where

-- References
-- http://c2.com/cgi/wiki?ExpressionProblem
-- http://koerbitz.me/posts/Solving-the-Expression-Problem-in-Haskell-and-Java.html
-- http://koerbitz.me/posts/Sum-Types-Visitors-and-the-Expression-Problem.html
-- https://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf
-- https://oleksandrmanzyuk.wordpress.com/2014/06/18/from-object-algebras-to-finally-tagless-interpreters-2/
-- https://www.andres-loeh.de/OpenDatatypes.pdf

-- We start with two shapes - square and circle - and one operation - area

-- Expression problem (1)
-- Using single ADT
--
-- Can add new functions: just create a new function, e.g. "perimeter"
-- 
-- Can't add new shapes: "Shape" ADT and existing functions are closed
{-
data Shape = Square Double | Circle Double
           deriving Show

area :: Shape -> Double
area s =
  case s of
       Square side -> side * side
       Circle radius -> pi * radius * radius

-- New operation
perimeter :: Shape -> Double
perimeter s =
  case s of
       Square side -> 4.0 * side
       Circle radius -> 2 * pi * radius

main :: IO ()
main = do
  let shapes = [Square 10.0, Circle 10.0]
  print $ map area shapes
  print $ map perimeter shapes
-}

-- Expression problem (2)
-- Using type classes and separate ADT for each shape
--
-- Straightforward to add new functions: declare a new type class and
-- implement an instance for each shape
-- 
-- Straightforward to add new shapes: declare a new ADT and implement existing
-- type classes
--
-- Problem: Heterogeneous lists of shapes no longer possible
{-
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

-- New operation
class Perimeter s where
  perimeter :: s -> Double
instance Perimeter Square where
  perimeter (Square side) = 4.0 * side
instance Perimeter Circle where
  perimeter (Circle radius) = 2.0 * pi * radius

-- New shape
data Rectangle = Rectangle Double Double
               deriving Show

instance Area Rectangle where
  area (Rectangle width height) = width * height

instance Perimeter Rectangle where
  perimeter (Rectangle width height) = 2.0 * width + 2.0 * height

main :: IO ()
main = do
  let
    squares = [Square 10.0, Square 20.0]
    circles = [Circle 10.0, Circle 20.0]
    rectangles = [Rectangle 10.0 20.0, Rectangle 20.0 30.0]
  print $ map area squares
  print $ map perimeter squares
  print $ map area circles
  print $ map perimeter circles
  print $ map area rectangles
  print $ map perimeter rectangles
-}

-- Expression problem (3)
-- Existential quantification

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

-- New operation: introduce new version of Shape
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

-- New shape: introduce new ADT and instances, new version of Shape not required
data Rectangle = Rectangle Double Double
               deriving Show
instance Area Rectangle where
  area (Rectangle width height) = width * height
instance Perimeter Rectangle where
  perimeter (Rectangle width height) = 2.0 * width + 2.0 * height

main :: IO ()
main = do
  let shapes = [
        ShapeV2 $ Square 10.0,
        ShapeV2 $ Circle 10.0,
        ShapeV2 $ Rectangle 10.0 20.0
        ]
  print $ map area shapes
  print $ map perimeter shapes
