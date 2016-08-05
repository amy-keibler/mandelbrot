module Mandelbrot.Algorithms.EscapeTime.InternalSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mandelbrot.Algorithms.EscapeTime.Internal
import Mandelbrot.Coloring (maxDepth)

import Data.Complex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lessThanRadius" $
    it "should determine if the distance is less than the radius" $ property
      (\x y -> lessThanRadius ((x :: Double) :+ y) `shouldBe` pythagoreanLT x y)
  describe "zSeries" $ do
    it "should be larger than the max depth" $ property
      (\x y -> length (take (maxDepth + 1) $ zSeries ((x :: Double) :+ y)) `shouldBe` maxDepth + 1)
    it "should produce the next element in the list by squaring and adding c" $ property
      zSeriesInduction

pythagoreanLT :: (Ord a, RealFloat a) => a -> a -> Bool
pythagoreanLT x y = sqrt (x ** 2 + y ** 2) < radius

zSeriesInduction :: Complex Double -> Int -> Bool
zSeriesInduction c num = zSeries c !! (abs num + 1) `nanInfComparesEqTo` (zSeries c !! abs num ^ (2 :: Integer) + c)
  where nanInfComparesEqTo (x :+ y) (x' :+ y')
          | isNaN x && isNaN x' = True
          | otherwise = x == x' && y == y'
