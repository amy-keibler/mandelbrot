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

data Resolution = Resolution { x :: Int
                             , y :: Int
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

generateComplexRange :: (RealFloat a, Enum a) => a -> a -> Complex a -> a -> a -> [Complex a]
generateComplexRange xRes yRes corner xDiff yDiff= [corner + offset x y |
                                                    x <- [1..xRes],
                                                    y <- [1..yRes]]
  where offset x y = ((x-1) * xDiff) :+ (y-1) * yDiff
