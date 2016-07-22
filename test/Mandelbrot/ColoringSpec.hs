module Mandelbrot.ColoringSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mandelbrot.Coloring (cppAlgorithm)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ccpAlgorithm" $ do
    it "should match the behavior of the cpp implementation" $ property
      (\x -> (cppAlgorithm . abs) x `shouldBe` (cppCleanRoomImpl . abs) x)


cppCleanRoomImpl :: Int -> (Int, Int, Int)
cppCleanRoomImpl num = (8 * num `mod` 256, 16 * num `mod` 256, 32 * num `mod` 256)
