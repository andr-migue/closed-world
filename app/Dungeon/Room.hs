module Dungeon.Room
  ( roomShell
  , roomPlaque
  , roomTorches
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Dungeon.Layout
import Dungeon.Random (hash)
import Dungeon.Textures
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Color (scaleRGB)
import Prolog.Protocol
import Sprites.Sprite
import Sprites.Torches
import Style.Colors (stone)

roomShell :: Canvas s -> Look -> Room -> ST s ()
roomShell cv lk r = when (inView cv x y roomW roomH) $ do
  let p px py c = plot cv px py (lookTone lk c)
  forM_ [y + 11 .. y + 25] $ \py -> forM_ [x + 2 .. x + roomW - 3] $ \px -> do
    let shadow
          | py == y + 11 = 0.55
          | py == y + 12 = 0.78
          | px == x + 2 || px == x + roomW - 3 = 0.8
          | otherwise = 1
    p px py (scaleRGB shadow (floorColor px py))
  forM_ [y + 3 .. y + 10] $ \py -> forM_ [x + 2 .. x + roomW - 3] $ \px -> do
    let brick = brickColor (y + 3) px py
    p px py (if py == y + 10 then scaleRGB 0.7 brick else brick)
  forM_ [x .. x + roomW - 1] $ \px -> do
    p px y outlineInk
    p px (y + 1) (if hash px y 9 `mod` 5 == 0 then stone 3 else stone 4)
    p px (y + 2) (stone 3)
    p px (y + roomH - 2) (stone 3)
    p px (y + roomH - 1) outlineInk
  forM_ [y .. y + roomH - 1] $ \py -> do
    p x py outlineInk
    p (x + 1) py (if py < y + 3 then stone 4 else stone 3)
    p (x + roomW - 2) py (if py < y + 3 then stone 4 else stone 3)
    p (x + roomW - 1) py outlineInk
  where (x, y) = origin r

roomPlaque :: Canvas s -> Look -> Room -> Int -> ST s ()
roomPlaque cv lk r textLength = when (textLength > 0) $ do
  let (x0, y0, w) = plaqueArea r textLength
      t = lookTone lk
  forM_ [x0 .. x0 + w - 1] $ \px -> do
    glow cv px y0 (t (40, 30, 30))
    glow cv px (y0 + 1) (t (30, 22, 24))
  forM_ [x0, x0 + w - 1] $ \px -> do
    glow cv px y0 (t (232, 168, 60))
    glow cv px (y0 + 1) (t (168, 100, 36))

roomTorches :: Canvas s -> Int -> Look -> Room -> ST s ()
roomTorches cv time lk r = forM_ (sconces r) $ \s -> do
  stamp cv (lookTone lk) (sconceX s - 1) (sconceY s + 3) sconce
  if lookLit lk
    then stampGlow cv id (sconceX s - 1) (sconceY s) (sconceFlames !! ((time `div` 2 + hash (sconceX s) (roomId r) 1) `mod` 4))
    else stamp cv (lookTone lk) (sconceX s - 1) (sconceY s) sconceCold
