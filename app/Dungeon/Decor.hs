module Dungeon.Decor (decorate) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST
import qualified Data.Text as T
import Dungeon.Layout
import Dungeon.Random
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Color (scaleRGB)
import Prolog.Protocol
import Sprites.Furniture
import Sprites.Mushrooms
import Sprites.Remains
import Sprites.Sprite
import Sprites.WallDecor
import Style.Colors (stone)

decorate :: Canvas s -> Look -> Room -> T.Text -> Int -> ST s ()
decorate cv lk r kind textLength = when (inView cv x y roomW roomH) $ do
  cobwebs cv lk r free
  banners cv lk r kind free
  cracks cv lk r free
  chain cv lk r free
  moss cv lk r
  floorProps cv lk r
  puddle cv lk r
  where
    (x, y) = origin r
    (x0, _, w) = plaqueArea r textLength
    free left width = left + width <= x0 - 1 || left >= x0 + w + 1

type Free = Int -> Int -> Bool

cobwebs :: Canvas s -> Look -> Room -> Free -> ST s ()
cobwebs cv lk r free = do
  when (chance 55 (roomId r) 2 0 && free (x + 2) 6) $
    stampBlend cv 0.5 (lookTone lk) (x + 2) (y + 3) cobweb
  when (chance 45 (roomId r) 3 0 && free (x + roomW - 8) 6) $
    stampBlend cv 0.5 (lookTone lk) (x + roomW - 8) (y + 3) (mirrored cobweb)
  where (x, y) = origin r

banners :: Canvas s -> Look -> Room -> T.Text -> Free -> ST s ()
banners cv lk r kind free = forM_ [(x + 10, 4), (x + roomW - 17, 5)] $ \(bx, salt) ->
  when (chance 40 (roomId r) salt 0 && free bx 7 && kind /= T.pack "start") $
    stamp cv (lookTone lk) bx (y + 3) wallBanner
  where (x, y) = origin r

cracks :: Canvas s -> Look -> Room -> Free -> ST s ()
cracks cv lk r free = forM_ [0 .. 2 :: Int] $ \i -> do
  let cx = x + 3 + hash (roomId r) i 31 `mod` (roomW - 6)
  when (chance 50 (roomId r) i 32 && free cx 1) $
    forM_ [0 .. 3] $ \k -> plot cv (cx + (k `mod` 2)) (y + 4 + k) (lookTone lk (stone 1))
  where (x, y) = origin r

chain :: Canvas s -> Look -> Room -> Free -> ST s ()
chain cv lk r free = when (chance 35 (roomId r) 6 0 && free cx 1) $
  forM_ [0 .. 4] $ \k -> plot cv cx (y + 3 + k) (lookTone lk (if even k then stone 5 else (94, 102, 130)))
  where
    (x, y) = origin r
    cx = x + 12 + hash (roomId r) 6 1 `mod` 12

moss :: Canvas s -> Look -> Room -> ST s ()
moss cv lk r = forM_ [x + 2 .. x + roomW - 3] $ \mx -> do
  when (hash mx (roomId r) 41 `mod` 7 == 0) $ plot cv mx (y + 10) (lookTone lk (58, 142, 92))
  when (hash mx (roomId r) 42 `mod` 13 == 0) $ plot cv mx (y + 9) (lookTone lk (30, 86, 60))
  when (hash mx (roomId r) 43 `mod` 17 == 0) $ plot cv mx (y + 11) (lookTone lk (58, 142, 92))
  where (x, y) = origin r

floorProps :: Canvas s -> Look -> Room -> ST s ()
floorProps cv lk r = forM_ spots $ \(sx, sy, salt) ->
  when (not (odd salt && hasCenterpiece) && chance (if salt < 2 then 70 else 40) (roomId r) salt 50) $ do
    let choice = hash (roomId r) salt 51 `mod` length props
        (s, glowing) = props !! choice
        left = if odd salt then sx - spriteWidth s else sx
    if glowing
      then stampGlow cv (if lookLit lk then id else scaleRGB 0.6) left sy s
      else stamp cv (lookTone lk) left sy s
  where
    (x, y) = origin r
    spots = [ (x + 3, y + 12, 0), (x + roomW - 3, y + 12, 1), (x + 3, y + 18, 2), (x + roomW - 3, y + 18, 3 :: Int) ]
    props = [ (barrel, False), (crate, False), (urn, False), (bones, False)
            , (skullSmall, False), (mushrooms, True), (crate, False), (barrel, False) ]
    hasCenterpiece = roomKind r `elem` map T.pack ["treasure", "trap", "dead_end", "exit"]

puddle :: Canvas s -> Look -> Room -> ST s ()
puddle cv lk r = when (chance 40 (roomId r) 9 0) $
  forM_ [(-2) .. 3] $ \k -> forM_ [0, 1] $ \j ->
    unless (j == 1 && (k == -2 || k == 3)) $
      plot cv (wx + k) (wy + j) (lookTone lk (water k j))
  where
    (x, y) = origin r
    wx = x + 9 + hash (roomId r) 9 1 `mod` 16
    wy = y + 14 + hash (roomId r) 9 2 `mod` 7
    water k j
      | j == 0 && k == 0 = (150, 200, 230)
      | j == 0 = (74, 124, 168)
      | otherwise = (36, 64, 104)
