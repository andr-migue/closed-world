module Dungeon.Textures
  ( outlineInk
  , voidRock
  , floorColor
  , cobbleColor
  , brickColor
  ) where

import Control.Monad (forM_)
import Control.Monad.ST
import Dungeon.Random (hash)
import Pixels.Canvas
import Style.Colors (RGB, stone)

outlineInk :: RGB
outlineInk = (26, 20, 36)

voidRock :: Canvas s -> Int -> Int -> Int -> Int -> ST s ()
voidRock cv x0 y0 w h = forM_ [y0 .. y0 + h - 1] $ \y -> forM_ [x0 .. x0 + w - 1] $ \x -> do
  let v = hash x y 7 `mod` 100
      c | v < 4     = (24, 22, 34)
        | v < 14    = (17, 16, 26)
        | otherwise = (12, 11, 19)
  plot cv x y c

floorColor :: Int -> Int -> RGB
floorColor x y
  | lx == 0 || ly == 0 = (50, 43, 52)
  | crack && (lx == ly + 1 || lx == ly + 2) && ly < 3 = (60, 52, 60)
  | moss && lx == 1 && ly == 3 = (58, 112, 74)
  | lx == 1 || ly == 1 = lift 9 base
  | otherwise = base
  where
    ty = y `div` 4
    shift = if odd ty then 3 else 0
    tx = (x + shift) `div` 6
    lx = (x + shift) `mod` 6
    ly = y `mod` 4
    base = case hash tx ty 11 `mod` 7 of
      0 -> (80, 70, 74)
      1 -> (96, 85, 86)
      2 -> (88, 78, 80)
      3 -> (104, 93, 92)
      4 -> (92, 82, 84)
      5 -> (84, 74, 78)
      _ -> (98, 88, 90)
    crack = hash tx ty 13 `mod` 11 == 0
    moss  = hash tx ty 17 `mod` 9 == 0

cobbleColor :: Int -> Int -> RGB
cobbleColor x y
  | lx == 0 || ly == 0 = (44, 38, 46)
  | lx == 1 && ly == 1 = lift 10 base
  | otherwise = base
  where
    ty = y `div` 3
    shift = if odd ty then 2 else 0
    tx = (x + shift) `div` 4
    lx = (x + shift) `mod` 4
    ly = y `mod` 3
    base = case hash tx ty 23 `mod` 4 of
      0 -> (70, 62, 68)
      1 -> (80, 71, 76)
      2 -> (74, 66, 72)
      _ -> (86, 77, 80)

brickColor :: Int -> Int -> Int -> RGB
brickColor top x y
  | ly == 2 = stone 1
  | lx == 0 = stone 1
  | ly == 0 = if hash bx by 5 `mod` 3 == 0 then stone 5 else stone 4
  | otherwise = case hash bx by 3 `mod` 5 of
      0 -> stone 2
      1 -> (66, 70, 98)
      _ -> stone 3
  where
    ry = y - top
    by = ry `div` 3
    shift = if odd by then 3 else 0
    bx = (x + shift) `div` 6
    lx = (x + shift) `mod` 6
    ly = ry `mod` 3

lift :: Int -> RGB -> RGB
lift n (r, g, b) = (r + n, g + n, b + n)
