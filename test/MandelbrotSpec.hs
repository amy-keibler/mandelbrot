module MandelbrotSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mandelbrot
import Data.Complex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateComplexRange" $ do
    it "should generate an empty list if resolution is zero" $
      generateComplexRange 0 0 (0 :+ 0) 0.1 0.1 `shouldBe` []
    it "should generate a four element list if resolution is 2x2" $
      generateComplexRange 2 2 (0 :+ 0) 0.1 0.1 `shouldBe` [0 :+ 0, 0 :+ 0.1,
                                                            0.1 :+ 0, 0.1 :+ 0.1]
  describe "curry3" $
    it "should curry a function that takes a tripe to take three arguments" $ property
    (\x y z -> curriedFn x y z `shouldBe` (x + y + z))
  describe "uncurry3" $
    it "should uncurry a function that takes three arguments to take a triple" $ property
    (\x y z -> uncurriedFn (x, y, z) `shouldBe` (x + y + z))

curriedFn :: Int -> Int -> Int -> Int
curriedFn = curry3 (\(a, b, c) -> a + b + c)

uncurriedFn :: (Int, Int, Int) -> Int
uncurriedFn = uncurry3 (\a b c -> a + b + c)
