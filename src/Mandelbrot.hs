module Mandelbrot (generateComplexRange) where

import Data.Complex

generateComplexRange :: (RealFloat a, Enum a) => a -> a -> Complex a -> a -> a -> [Complex a]
generateComplexRange xRes yRes corner xDiff yDiff= [corner + offset x y |
                                                    x <- [1..xRes],
                                                    y <- [1..yRes]]
  where offset x y = ((x-1) * xDiff) :+ (y-1) * yDiff
