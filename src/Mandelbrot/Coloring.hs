module Mandelbrot.Coloring (cppAlgorithm
                           , greenColoring
                           , grayColoring) where

type Coloring = Int -> (Int, Int, Int)

cppAlgorithm :: Coloring
cppAlgorithm num = (valBy 8, valBy 16, valBy 32)
  where valBy = (`mod` 256) . (* num)

greenColoring :: Coloring
greenColoring num = (0, num `mod` 256, 0)

grayColoring :: Coloring
grayColoring num = (num `mod` 256, num `mod` 256, num `mod` 256)
