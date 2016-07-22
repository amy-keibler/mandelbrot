module Main where

import Mandelbrot.Coloring (cppAlgorithm, greenColoring, grayColoring)
import Mandelbrot.Algorithms.EscapeTime (numIterations)

import Data.Complex
import Codec.Picture

main :: IO ()
main = writePng "/tmp/test.png" $ generateMandelbrotImage (0.338694 :+ 0.072486) 1024 768 1.48582e-7 2.018253e-7

generateMandelbrotImage :: (RealFloat a) => Complex a -> Int -> Int -> a -> a -> Image PixelRGB8
generateMandelbrotImage corner xRes yRes xDiff yDiff = generateImage pixelRenderer xRes yRes
    where pixelRenderer x y = (toPixel . greenColoring . numIterations) $ toCoordinate x y
          toCoordinate x y = corner + ((fromIntegral x * xDiff) :+ (fromIntegral y * yDiff))
          toPixel (r, g, b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
