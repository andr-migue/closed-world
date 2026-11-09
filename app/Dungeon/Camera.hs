module Dungeon.Camera
  ( followCamera
  , cameraCorner
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
    x = clampTo (round fx - viewW `div` 2 + panX) viewW (lyWidth ly)
    y = clampTo (round fy - viewH `div` 2 + panY) viewH (lyHeight ly)
    clampTo c view world
      | world <= view = (world - view) `div` 2
      | otherwise     = max (-margin) (min (world - view + margin) c)
