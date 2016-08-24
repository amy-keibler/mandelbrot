{-# LANGUAGE DeriveGeneric #-}
module Mandelbrot (generateComplexRange
                  , ImageData (..)
                  , Resolution (..)
                  , coloringFrom
                  , curry3
                  , uncurry3
                  , toCoordinate) where

import GHC.Generics
import Data.Aeson
import Data.Complex

import Mandelbrot.Coloring

data Resolution = Resolution { xRes :: Int
                             , yRes :: Int
                             } deriving (Generic, Show)

instance FromJSON Resolution
instance ToJSON Resolution

data ImageData = ImageData { filename :: String
                           , xStart :: Double
                           , yStart :: Double
                           , resolution :: Resolution
                           , coloring :: String
                           , xDiff :: Double
                           , yDiff :: Double
                           } deriving (Generic, Show)

instance FromJSON ImageData
instance ToJSON ImageData

coloringFrom :: String -> Coloring
coloringFrom name = case name of "diverse" -> diverseColoring
                                 "green" -> greenColoring
                                 _ -> grayColoring

curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 func x y z = func (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (x, y, z) = func x y z

toCoordinate :: ImageData -> Int -> Int -> Complex Double
toCoordinate image xCoord yCoord = (xStart image :+ yStart image) + ((fromIntegral xCoord * xDiff image) :+ (fromIntegral yCoord * yDiff image))

generateComplexRange :: ImageData -> [Complex Double]
generateComplexRange imageData = [corner + offset x y |
                                  x <- [1 .. (xRes $ resolution imageData)],
                                  y <- [1 .. (yRes $ resolution imageData)]]
  where corner = xStart imageData :+ yStart imageData
        offset x y = ((fromIntegral x - 1) * xDiff imageData) :+ (fromIntegral y-1) * yDiff imageData
