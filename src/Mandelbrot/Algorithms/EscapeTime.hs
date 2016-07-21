module Mandelbrot.Algorithms.EscapeTime (numIterations) where

import Data.Complex

maxIterations :: Integral a => a
maxIterations = 256

radius :: RealFloat a => a
radius = 3

numIterations :: RealFloat a => Complex a -> Int
numIterations c = length $ take maxIterations $ takeWhile (lessThanRadius radius) $ zSeries c

lessThanRadius :: (Floating a, Ord a) => a -> Complex a -> Bool
lessThanRadius r z = realPart z ^ (2 :: Integer) + imagPart z ^ (2 :: Integer) < r ^ (2 :: Integer)

zSeries :: RealFloat a => Complex a -> [Complex a]
zSeries c = iterate nextZ (0 :+ 0)
  where nextZ z = z ^ (2 :: Integer) + c
