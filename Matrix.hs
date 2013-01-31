{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Matrix
    ( Matrix(..)
    , toList
    , toMatrix
    , identity
    , translation
    , scaling
    , rotation
    , translationBack
    , scalingSame
    , transform
    ) where

import Point

type D = Double
data Matrix = Matrix D D D D D D
              deriving(Show,Eq)

instance Num Matrix where
    lhs + rhs = toMatrix $ zipWith (+) (toList lhs) (toList rhs)
    lhs - rhs = toMatrix $ zipWith (-) (toList lhs) (toList rhs)
    (Matrix a1 a2 a3 a4 a5 a6) * (Matrix b1 b2 b3 b4 b5 b6)
        = Matrix (a1*b1+a2*b4) (a1*b2+a2*b5) (a1*b3+a2*b6+a3)
                    (a4*b1+a5*b4) (a4*b2+a5*b5) (a4*b3+a5*b6+a6)
    signum _ = identity
    abs = toMatrix . map abs . toList
    fromInteger x = let x' = fromInteger x in
                    Matrix x' 0 0 0 x' 0

toList :: Matrix -> [D]
toList (Matrix a b c d e f) = [a,b,c,d,e,f]

toMatrix :: [D] -> Matrix
toMatrix [a,b,c,d,e,f] = Matrix a b c d e f

identity :: Matrix
identity = Matrix 1 0 0 0 1 0

translation :: Point -> Matrix
translation (x,y) = Matrix 1 0 x 0 1 y

scaling :: D -> D -> Matrix
scaling x y = Matrix x 0 0 0 y 0

rotation :: D -> Matrix
rotation x = Matrix (cos x) (-sin x) 0 (sin x) (cos x) 0

translationBack :: Point -> Matrix -> Matrix
translationBack (x,y) m = (translation (x,y)) * m * (translation (-x,-y))

scalingSame :: D -> Matrix
scalingSame x = scaling x x

class Transformable a where
    transform :: Matrix -> a -> a

instance Transformable Point where
    transform (Matrix a b c d e f) (x,y) = (a*x+b*y+c, d*x+e*y+f)

instance Transformable Polygon where
    transform m = map (transform m)
