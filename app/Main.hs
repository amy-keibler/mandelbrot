module Main where

import Mandelbrot.Coloring
import Mandelbrot.Algorithms.EscapeTime (numIterations)

import Data.Complex
import Codec.Picture

main :: IO ()
main = writePng "/tmp/test.png" $ generateMandelbrotImage ((-2.5) :+ (-2)) 1000 800 0.005 0.005

generateMandelbrotImage :: (RealFloat a) => Complex a -> Int -> Int -> a -> a -> Image PixelRGB8
generateMandelbrotImage corner xRes yRes xDiff yDiff = generateImage pixelRenderer xRes yRes
    where pixelRenderer x y = (toPixel . diverseColoring . numIterations) $ toCoordinate x y
          toCoordinate x y = corner + ((fromIntegral x * xDiff) :+ (fromIntegral y * yDiff))
          toPixel (r, g, b) = PixelRGB8 r g b
