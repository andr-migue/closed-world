module Dungeon.Features (feature) where

import Control.Monad (forM_, unless)
import Control.Monad.ST
import qualified Data.Text as T
import Dungeon.Layout
import Dungeon.Random (hash)
import Dungeon.Textures (outlineInk)
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Color (Tone, scaleRGB)
import Prolog.Protocol
import Sprites.Chest
import Sprites.Remains (skullSmall)
import Sprites.Sprite
import Style.Colors (stone)

feature :: Canvas s -> Look -> Room -> T.Text -> ST s ()
feature cv lk r kind = case T.unpack kind of
  "start"    -> entranceStairs cv lk r
  "treasure" -> treasureChest cv lk r
  "trap"     -> spikePit cv lk r
  "dead_end" -> rubblePile cv lk r
  _          -> pure ()

entranceStairs :: Canvas s -> Look -> Room -> ST s ()
entranceStairs cv lk r = do
  forM_ [0 .. 3] $ \s -> forM_ [x + roomW - 13 .. x + roomW - 5] $ \px -> do
    plot cv px (y + 3 + s * 2) (t (scaleRGB (0.35 + 0.15 * fromIntegral s) (stone 5)))
    plot cv px (y + 4 + s * 2) (t (scaleRGB (0.3 + 0.12 * fromIntegral s) (stone 2)))
  forM_ [y + 3 .. y + 10] $ \py -> do
    plot cv (x + roomW - 14) py (t (stone 5))
    plot cv (x + roomW - 4) py (t outlineInk)
  forM_ [y + 12 .. y + 24] $ \py -> forM_ [x + 13 .. x + 22] $ \px -> do
    let c | px == x + 13 || px == x + 22 = (104, 22, 42)
          | px == x + 14 || px == x + 21 = (232, 168, 60)
          | (px + py) `mod` 4 == 0 = (150, 34, 56)
          | otherwise = (182, 42, 64)
    plot cv px py (t c)
  where
    (x, y) = origin r
    t = lookTone lk

treasureChest :: Canvas s -> Look -> Room -> ST s ()
treasureChest cv lk r = do
  stamp cv (lookTone lk) (cx - 5) (cy - 4) chestOpen
  forM_ [0 .. 5 :: Int] $ \i -> do
    let gx = cx - 8 + hash (roomId r) i 61 `mod` 16
        gy = cy + 4 + hash (roomId r) i 62 `mod` 4
    glow cv gx gy (255, 222, 130)
  where (cx, cy) = chestCenter r

spikePit :: Canvas s -> Look -> Room -> ST s ()
spikePit cv lk r = do
  forM_ [cy - 3 .. cy + 3] $ \py -> forM_ [cx - 6 .. cx + 5] $ \px -> do
    let edge = py == cy - 3 || px == cx - 6 || px == cx + 5
    plot cv px py (t (if edge then outlineInk else (14, 10, 20)))
  forM_ [cx - 4, cx - 1, cx + 2] $ \sx -> forM_ [cy - 1, cy + 2] $ \sy -> do
    plot cv sx (sy - 1) (t (216, 224, 242))
    plot cv sx sy (t (152, 164, 190))
    plot cv (sx + 1) sy (t (94, 102, 130))
  stamp cv t (cx - 12) (cy + 2) skullSmall
  where
    (cx, cy) = trapCenter r
    t = lookTone lk

rubblePile :: Canvas s -> Look -> Room -> ST s ()
rubblePile cv lk r = forM_ [0 .. 13 :: Int] $ \i -> do
  let rx = x + 21 + hash (roomId r) i 71 `mod` 9
      ry = y + 13 + hash (roomId r) i 72 `mod` 8
      size = 2 + hash (roomId r) i 73 `mod` 3
  rock cv (lookTone lk) rx ry size
  where (x, y) = origin r

rock :: Canvas s -> Tone -> Int -> Int -> Int -> ST s ()
rock cv t x y s = do
  forM_ [0 .. s] $ \j -> forM_ [0 .. s + 1] $ \i -> do
    let corner = (i == 0 || i == s + 1) && (j == 0 || j == s)
        c | j == 0 || i == 0 = stone 5
          | j == s || i == s + 1 = stone 2
          | otherwise = stone 4
    unless corner (plot cv (x + i) (y + j) (t c))
  forM_ [1 .. s] $ \i -> plot cv (x + i) (y + s + 1) (t outlineInk)
