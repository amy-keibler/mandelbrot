module MandelbrotSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mandelbrot (numIterations)
import Data.Complex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "numIterations" $ do
    it "it matches the cpp project definition" $ property $
      \a -> numIterations (a :: Complex Double) `shouldBe` cppMandelbrot a


cppMandelbrot :: Complex Double -> Int
cppMandelbrot c = length $ take 256 $ takeWhile (lessThanRadius 3) $ iterate (nextZ c) (0.0 :+ 0.0)

nextZ :: Complex Double -> Complex Double -> Complex Double
nextZ c z = z * z + c

lessThanRadius :: Double -> Complex Double -> Bool
lessThanRadius r z = sqrt (realPart z ^2 + imagPart z ^2) < r
