module Mandelbrot.Coloring (diverseColoring
                           , greenColoring
                           , grayColoring
                           , maxDepth) where

import Data.Word

-- 1 byte per color channel
maxDepth :: Integral a => a
maxDepth = fromIntegral (maxBound :: Word8) + 1

type Coloring = Int -> (Word8, Word8, Word8)

diverseColoring :: Coloring
diverseColoring num = (valBy 8, valBy 16, valBy 32)
  where valBy = wrapNum . (* num)

greenColoring :: Coloring
greenColoring num = (0, wrapNum num, 0)

grayColoring :: Coloring
grayColoring num = (wrappedNum, wrappedNum, wrappedNum)
  where wrappedNum = wrapNum num

wrapNum :: Int -> Word8
wrapNum = fromIntegral . (`mod` maxDepth)
