{-# LANGUAGE BangPatterns #-}
module Pixels.Light
  ( Light (..)
  , Lighting (..)
  , shade
  ) where

import Data.Bits ((.&.))
import qualified Data.Vector.Unboxed as U
import Pixels.Canvas (Picture (..), Viewport (..))
import Pixels.Color

data Light = Light
  { lightX      :: !Double
  , lightY      :: !Double
  , lightRadius :: !Double
  , lightColor  :: !(Double, Double, Double)
  , lightPower  :: !Double
  }

data Lighting = Lighting
  { ambient  :: Double
  , lights   :: [Light]
  , vignette :: Bool
  }

shade :: Viewport -> Lighting -> Picture -> U.Vector Pixel
shade port lighting pic = U.generate (w * h) px
  where
    w = portW port
    h = portH port
    near l = lightX l + lightRadius l >= fromIntegral (portX port)
          && lightX l - lightRadius l <= fromIntegral (portX port + w)
          && lightY l + lightRadius l >= fromIntegral (portY port)
          && lightY l - lightRadius l <= fromIntegral (portY port + h)
    visible = filter near (lights lighting)
    base = ambient lighting
    px i
      | U.unsafeIndex (picGlowing pic) i = edgeFade lx ly (U.unsafeIndex (picColors pic) i)
      | otherwise =
          let (r, g, b) = unpack (U.unsafeIndex (picColors pic) i)
              wx = fromIntegral (lx + portX port) + 0.5
              wy = fromIntegral (ly + portY port) + 0.5
              !(lr, lg, lb) = foldr (addLight wx wy) (base, base, base * 1.08) visible
              d = dither lx ly
              step v = max 0 (fromIntegral (floor (v * 10 + d) :: Int) / 10)
              channel c v = round (fromIntegral c * min 1.7 (step v)) :: Int
          in edgeFade lx ly (pack (channel r lr, channel g lg, channel b lb))
      where
        lx = i `mod` w
        ly = i `div` w
    edgeFade lx ly p
      | not (vignette lighting) = p
      | otherwise =
          let f = min (edge lx w) (edge ly h)
          in if f >= 1 then p else pack (scaleRGB (0.55 + 0.45 * f) (unpack p))
    edge v n = min 1 (fromIntegral (min v (n - 1 - v)) / 6 :: Double)

addLight :: Double -> Double -> Light -> (Double, Double, Double) -> (Double, Double, Double)
addLight wx wy l (!ar, !ag, !ab)
  | d2 >= r * r = (ar, ag, ab)
  | otherwise = (ar + k * cr, ag + k * cg, ab + k * cb)
  where
    dx = wx - lightX l
    dy = wy - lightY l
    d2 = dx * dx + dy * dy
    r = lightRadius l
    f = 1 - sqrt d2 / r
    k = f * f * lightPower l
    (cr, cg, cb) = lightColor l

dither :: Int -> Int -> Double
dither x y = U.unsafeIndex bayer ((y .&. 3) * 4 + (x .&. 3))

bayer :: U.Vector Double
bayer = U.fromList [ (v + 0.5) / 16 - 0.5 | v <- [0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5] ]
