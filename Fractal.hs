module Fractal
    ( Initiater(..)
    , Generator(..)
    , WithPolygon(..)
    , applyGenerator
    , applyGenerators
    ) where

import Data.Tuple(swap)
import Point
import Matrix

data Initiater = Initiater Polygon
                 deriving(Show,Eq)

data Generator = Generator Polygon 
                 deriving(Show,Eq)

class WithPolygon a where
    polygon :: a -> Polygon

instance WithPolygon Initiater where
    polygon (Initiater x) = x

instance WithPolygon Generator where
    polygon (Generator x) = x

unique :: (Eq a) => [a] -> [a]
unique (x:y:zs) | x==y      = unique (y:zs)
                | otherwise = x : unique (y:zs)
unique (x:[]) = [x]
unique [] = []

asPairs :: Polygon -> [(Point,Point)]
asPairs (a:b:cs) = (a,b) : asPairs (b:cs)
asPairs (_:[])   = []

applyToPair :: Generator -> (Point,Point) -> Polygon
applyToPair (Generator gPoly) (a,b)
    = let radian = uncurry atan2 $ swap (b-a)
          scale = scalingSame $ distance a b
          translate = translation a
          rotate = rotation radian
          mat = translate * rotate * scale
      in  transform mat gPoly

applyGenerator :: Generator -> Initiater -> Initiater
applyGenerator gen
    = Initiater . unique . concatMap (applyToPair gen) . asPairs . polygon

applyGenerators :: [Generator] -> Initiater -> Initiater
applyGenerators gens
    = Initiater . concat . zipWith applyToPair gens' .  asPairs . polygon
      where gens' = concat $ repeat gens

