module Mandelbrot.Algorithms.EscapeTime (numIterations
                                        , maxIterations) where

import Data.Complex

maxIterations :: Integral a => a
maxIterations = 256

radius :: RealFloat a => a
radius = 3

numIterations :: RealFloat a => Complex a -> Int
numIterations = length . take maxIterations . takeWhile lessThanRadius . zSeries

lessThanRadius :: (RealFloat a, Ord a) => Complex a -> Bool
lessThanRadius z = (square . realPart) z + (square . imagPart) z < square radius
  where square = (** 2)

zSeries :: RealFloat a => Complex a -> [Complex a]
zSeries c = iterate nextZ c
  where nextZ z = (z ^ (2 :: Integer)) + c
