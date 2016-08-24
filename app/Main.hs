module Main where

import Mandelbrot
import Mandelbrot.Algorithms.EscapeTime (numIterations)

import Data.Aeson
import Codec.Picture
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = getArgs >>= handleFileName
  where handleFileName [filepath] = eitherDecode <$> B.readFile filepath >>= outputImage
        handleFileName _ = putStrLn "Please pass in the filename of the json config file."

outputImage :: Either String ImageData -> IO ()
outputImage (Left err) = putStrLn err
outputImage (Right image) = writePng (filename image) $ generateMandelbrotImage image

generateMandelbrotImage :: ImageData -> Image PixelRGB8
generateMandelbrotImage image = generateImage pixelRenderer (xRes $ resolution image) (yRes $ resolution image)
    where pixelRenderer xCoord yCoord = (toPixel . coloringFrom (coloring image) . numIterations) $ toCoordinate image xCoord yCoord
          toPixel = uncurry3 PixelRGB8
