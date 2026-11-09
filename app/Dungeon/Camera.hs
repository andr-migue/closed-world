module Dungeon.Camera
  ( followCamera
  , cameraCorner
  , clampPan
  ) where

import Dungeon.Layout

followCamera :: (Int, Int) -> (Double, Double) -> Maybe (Double, Double) -> (Double, Double)
followCamera _ target Nothing = target
followCamera (viewW, viewH) (tx, ty) (Just (cx, cy)) =
  (push cx tx (0.18 * fromIntegral viewW), push cy ty (0.15 * fromIntegral viewH))
  where
    push c t slack
      | t - c > slack = t - slack
      | c - t > slack = t + slack
      | otherwise     = c

cameraCorner :: Layout -> (Int, Int) -> (Double, Double) -> (Int, Int) -> (Int, Int)
cameraCorner ly (viewW, viewH) (fx, fy) (panX, panY) = (x, 2 * (y `div` 2))
  where
    x = clampTo (lyWidth ly) viewW (centered fx viewW + panX)
    y = clampTo (lyHeight ly) viewH (centered fy viewH + panY)

clampPan :: Layout -> (Int, Int) -> (Double, Double) -> (Int, Int) -> (Int, Int)
clampPan ly (viewW, viewH) (fx, fy) (panX, panY) = (fit (lyWidth ly) viewW fx panX, fit (lyHeight ly) viewH fy panY)
  where
    fit world view f p
      | world <= view = 0
      | otherwise     = clampTo world view (centered f view + p) - centered f view

centered :: Double -> Int -> Int
centered f view = round f - view `div` 2

clampTo :: Int -> Int -> Int -> Int
clampTo world view c
  | world <= view = (world - view) `div` 2
  | otherwise     = max (-margin) (min (world - view + margin) c)
