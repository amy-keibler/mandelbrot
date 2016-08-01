module Mandelbrot.Algorithms.EscapeTimeSpec (main, spec) where

import Test.Hspec

import Mandelbrot.Algorithms.EscapeTime (numIterations)
import Mandelbrot.Coloring (maxDepth)
import Data.Complex

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "numIterations" $ do
  it "should never converge for 0 :+ 0" $
    numIterations (0 :+ 0) `shouldBe` maxDepth
  it "should immediately converge for (-2.5) :+ (-2)" $
    numIterations ((-2.5) :+ (-2)) `shouldBe` 1
  it "should quickly converge for (-2.5) :+ (-1)" $
    numIterations ((-2.5) :+ (-1)) `shouldBe` 2
