module Mandelbrot.Algorithms.EscapeTime (numIterations) where

import Mandelbrot.Algorithms.EscapeTime.Internal

import Data.Complex

import Mandelbrot.Coloring (maxDepth)

numIterations :: RealFloat a => Complex a -> Int
numIterations = length . take maxDepth . takeWhile lessThanRadius . zSeries
