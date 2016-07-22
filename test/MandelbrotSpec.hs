module MandelbrotSpec (main, spec) where

import Test.Hspec

import Mandelbrot (generateComplexRange)
import Data.Complex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateComplexRange" $ do
    it "should generate an empty list if resolution is zero" $ do
      generateComplexRange 0 0 (0 :+ 0) 0.1 0.1 `shouldBe` []
    it "should generate a four element list if resolution is 2x2" $ do
      generateComplexRange 2 2 (0 :+ 0) 0.1 0.1 `shouldBe` [0 :+ 0, 0 :+ 0.1,
                                                            0.1 :+ 0, 0.1 :+ 0.1]
