{-# LANGUAGE TypeSynonymInstances #-}
module Point
    ( Point(..)
    , Polygon(..)
    , distance
    , line, lineR, lineC
    , triangle, triangleR, triangleC
    , square, squareR, squareC
    , hexagon, hexagonR, hexagonC
    ) where

type Point   = (Double,Double)
type Polygon = [Point]

instance Num Point where
    (x1,y1) + (x2,y2) = (x1+x2, y1+y2)
    (x1,y1) * (x2,y2) = (x1*x2, y1*y2)
    negate (x,y) = (-x, -y)
    abs (x,y) = (abs x, abs y)
    signum (x,y) = (x/x, y/y)
    fromInteger x = (fromInteger x, fromInteger x)

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

line  = [(0,0),(1,0)] :: Polygon
lineR = reverse line
lineC = line ++ lineR
        
triangle  = [(0,0),(0.5,sqrt(3)/2),(1,0),(0,0)] :: Polygon
triangleR = reverse triangle
triangleC = triangle ++ triangleR

square  = [(0,0),(0,1),(1,1),(1,0),(0,0)] :: Polygon
squareR = reverse square
squareC = square ++ squareR

h = 0.5 / tan (1/3*pi)
a = sqrt (h**2 + 0.5**2)
hexagon  = [(0,h),(0,h+a),(0.5,2*h+a),(1,h+a),(1,h),(0.5,0),(0,h)] :: Polygon
hexagonR = reverse hexagon
hexagonC = hexagon ++ hexagonR
