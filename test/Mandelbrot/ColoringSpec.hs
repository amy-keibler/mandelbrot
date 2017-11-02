module Mandelbrot.ColoringSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mandelbrot.Coloring
import Data.Word

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "diverseColoring" $ do
    it "should produce a zero value for zero input" $
      diverseColoring 0 `shouldBe` (0, 0, 0)
    it "should produce values that are *8, *16, and *32" $ property
      (\x -> diverseColoring x `shouldBe` (fromInteger $ toInteger x * 8 `mod` (maxDepth :: Integer),
                                           fromInteger $ toInteger x * 16 `mod` maxDepth,
                                           fromInteger $ toInteger x * 32 `mod` maxDepth))
    it "should wrap all numbers to the range [0, 256)" $ property
      (inRange . diverseColoring)
  describe "greenColoring" $ do
    it "should always have zero for red and blue channels" $ property
      (and . ([redZero, blueZero] <*>) . (:[]) . greenColoring)
    it "should always have green be the mod of the input with the max depth" $ property
      (\x -> (greenValue . greenColoring) x `shouldBe` x `mod` maxDepth)
    it "should wrap all numbers to the range [0, 256)" $ property
      (inRange . greenColoring)
  describe "grayColoring" $ do
    it "should always have equal values for the three channels" $ property
      (equalChannels . grayColoring)
    it "should always have green be the mod of the input with the max depth" $ property
      (\x -> (greenValue . greenColoring) x `shouldBe` x `mod` maxDepth)
    it "should wrap all numbers to the range [0, 256)" $ property
      (inRange . grayColoring)

inRange :: Integral a => (a, a, a) -> Bool
inRange (r, g, b) = zeroToMax r && zeroToMax g && zeroToMax b

zeroToMax :: Integral a => a -> Bool
zeroToMax num = intNum >= 0 && intNum < maxDepth
  where intNum = toInteger num

redZero :: (Word8, Word8, Word8) -> Bool
redZero (r, _, _) = r == 0

blueZero :: (Word8, Word8, Word8) -> Bool
blueZero (_, _, b) = b == 0

equalChannels :: (Word8, Word8, Word8) -> Bool
equalChannels (r, g, b) = r == g && g == b

greenValue :: Integral a => (Word8, Word8, Word8) -> a
greenValue (_, g, _) = fromIntegral g
