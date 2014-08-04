module Shapes
( Point(..)
 , Shape(..)
 , surface
 , nudge
 , baseCircle
 , baseRect 
 ) where 
 
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 

surface :: Shape -> Float
surface (Circle  _ r) = pi * r ^ 2 
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- nudge takes a shape, the amount to move it on the x axis and the amount to move it on the y axis 
--and then returns a new shape that has the same dimensions, only it's located somewhere else
--usage nudge: nudge (Circle (Point 34 34) 10) 5 10 , moves a circle with center in 34 34, and r= 10. Result: Circle (Point 39.0 44.0) 10.0
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point(x+a) (y+b)) r 
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

--base circle with center in 0,0 and r radius
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

--base rectangle with one corner in 0,0
baseRect :: Float -> Float -> Shape
baseRect width heigth = Rectangle (Point 0 0) (Point width heigth)