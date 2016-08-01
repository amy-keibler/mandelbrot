module Mandelbrot.Algorithms.EscapeTime (numIterations) where

import Data.Complex

import Mandelbrot.Coloring (maxDepth)

radius :: RealFloat a => a
radius = 3

numIterations :: RealFloat a => Complex a -> Int
numIterations = length . take maxDepth . takeWhile lessThanRadius . zSeries

lessThanRadius :: (RealFloat a, Ord a) => Complex a -> Bool
lessThanRadius z = pythagoreanLT (realPart z) (imagPart z) radius
  where pythagoreanLT a b c = a ** 2 + b ** 2 < c ** 2

zSeries :: RealFloat a => Complex a -> [Complex a]
zSeries c = iterate nextZ (0 :+ 0)
  where nextZ z = (z ^ (2 :: Integer)) + c
