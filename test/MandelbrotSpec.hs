module MandelbrotSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import System.Random

import Mandelbrot (numIterations)
import Data.Complex
import Data.List (genericLength)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "numIterations" $ do
    it "is undefined for numbers outside the range x [-2.5, 1], y [-1, 1]" $ do
      forAll (oneof [genBelowRange, genAboveRange]) $ \a -> numIterations (a :: Complex Double) `shouldBe` Nothing
    it "it matches the cpp project definition" $ do
      forAll genInRange $ \a -> numIterations (a :: Complex Double) `shouldBe` Just (cppMandelbrot a)


genInRange :: (Fractional a, Random a) => Gen (Complex a)
genInRange = do
  x <- choose (-2.5, 1)
  y <- choose (-1, 1)
  return (x :+ y)

genAboveRange :: Gen (Complex Double)
genAboveRange = do
  x <- choose (1.001, 1000)
  y <- choose (1.001, 1000)
  return (x :+ y)

genBelowRange :: Gen (Complex Double)
genBelowRange = do
  x <- choose (-1000, -2.499)
  y <- choose (-1000, -1.999)
  return (x :+ y)

cppMandelbrot :: Complex Double -> Integer
cppMandelbrot c = genericLength $ take 256 $ takeWhile (lessThanRadius 3) $ iterate (nextZ c) (0.0 :+ 0.0)

nextZ :: Complex Double -> Complex Double -> Complex Double
nextZ c z = z * z + c

lessThanRadius :: Double -> Complex Double -> Bool
lessThanRadius r z = sqrt (realPart z ^2 + imagPart z ^2) < r
