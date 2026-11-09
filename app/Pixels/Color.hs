module Pixels.Color
  ( Pixel
  , Tone
  , pack
  , unpack
  , mixRGB
  , scaleRGB
  , luma
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Style.Colors (RGB)

type Pixel = Int

type Tone = RGB -> RGB

pack :: RGB -> Pixel
pack (r, g, b) = (clamp8 r `shiftL` 16) + (clamp8 g `shiftL` 8) + clamp8 b

unpack :: Pixel -> RGB
unpack p = (p `shiftR` 16 .&. 255, p `shiftR` 8 .&. 255, p .&. 255)

mixRGB :: Double -> RGB -> RGB -> RGB
mixRGB a (r1, g1, b1) (r2, g2, b2) = (m r1 r2, m g1 g2, m b1 b2)
  where m p q = round (fromIntegral p * (1 - a) + fromIntegral q * a)

scaleRGB :: Double -> RGB -> RGB
scaleRGB f (r, g, b) = (s r, s g, s b)
  where s c = clamp8 (round (fromIntegral c * f))

luma :: RGB -> Int
luma (r, g, b) = (r * 3 + g * 6 + b) `div` 10

clamp8 :: Int -> Int
clamp8 = max 0 . min 255
