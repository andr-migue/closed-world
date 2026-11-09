module Dungeon.Corridor
  ( Corridor (..)
  , corridorWalls
  , corridorFloor
  , corridorTorch
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Dungeon.Textures
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Color (scaleRGB)
import Style.Colors (stone)

data Corridor = Corridor
  { corDoorX  :: Int
  , corChildX :: Int
  , corTop    :: Int
  , corBar    :: Int
  , corBottom :: Int
  , corLook   :: Look
  }

corridorWalls :: Canvas s -> Corridor -> ST s ()
corridorWalls cv c = do
  forM_ [lo .. hi] $ \px -> do
    p px (b - 6) outlineInk
    p px (b - 5) (stone 4)
    p px (b - 4) (brickColor (b - 4) px (b - 4))
    p px (b - 3) (brickColor (b - 4) px (b - 3))
    p px (b + 2) (stone 3)
    p px (b + 3) outlineInk
  column (corDoorX c) (corTop c) (b - 6)
  column (corChildX c) (b + 3) (corBottom c - 1)
  where
    p px py col = plot cv px py (lookTone (corLook c) col)
    b = corBar c
    lo = min (corDoorX c) (corChildX c) - 4
    hi = max (corDoorX c) (corChildX c) + 3
    column cx y1 y2 = forM_ [y1 .. y2] $ \py -> do
      p (cx - 4) py outlineInk
      p (cx - 3) py (stone 3)
      p (cx + 2) py (stone 3)
      p (cx + 3) py outlineInk

corridorFloor :: Canvas s -> Corridor -> ST s ()
corridorFloor cv c = do
  forM_ [corTop c - 2 .. b + 1] $ \py -> forM_ [corDoorX c - 2 .. corDoorX c + 1] $ \px ->
    cobble px py (if py < corTop c then 0.8 else 1)
  forM_ [b - 2 .. b + 1] $ \py -> forM_ [lo .. hi] $ \px ->
    cobble px py (if py == b - 2 then 0.7 else 1)
  forM_ [b - 2 .. corBottom c + 10] $ \py -> forM_ [corChildX c - 2 .. corChildX c + 1] $ \px ->
    cobble px py (if py < corBottom c then 1 else 0.5 + 0.05 * fromIntegral (py - corBottom c))
  forM_ [corBottom c .. corBottom c + 10] $ \py -> do
    plot cv (corChildX c - 3) py (t (stone (if py < corBottom c + 3 then 5 else 4)))
    plot cv (corChildX c + 2) py (t (stone (if py < corBottom c + 3 then 4 else 2)))
  where
    t = lookTone (corLook c)
    b = corBar c
    lo = min (corDoorX c) (corChildX c) - 2
    hi = max (corDoorX c) (corChildX c) + 1
    cobble px py shadow = plot cv px py (t (scaleRGB shadow (cobbleColor px py)))

corridorTorch :: Canvas s -> Int -> Corridor -> ST s ()
corridorTorch cv time c = when (hi - lo >= 24) $ do
  plot cv mid (y + 1) (lookTone (corLook c) (94, 102, 130))
  if lookLit (corLook c)
    then do
      glow cv mid y (if even (time `div` 2) then (255, 196, 86) else (255, 140, 52))
      glow cv mid (y - 1) (255, 140, 52)
    else plot cv mid y (lookTone (corLook c) (60, 52, 50))
  where
    lo = min (corDoorX c) (corChildX c)
    hi = max (corDoorX c) (corChildX c)
    mid = (lo + hi) `div` 2
    y = corBar c - 4
