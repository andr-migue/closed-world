module Dungeon.Actors (drawCast) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Dungeon.Cast
import Dungeon.Random (hash)
import Dungeon.Walking
import Pixels.Canvas
import Sprites.Ghost
import Sprites.Hero
import Sprites.Sprite

drawCast :: Canvas s -> Int -> Cast -> Double -> ST s ()
drawCast cv time cast torch = do
  forM_ (castHero cast) $ \a -> when (actorAlpha a > 0) (drawHero cv time a torch)
  forM_ (castGhost cast) $ \g -> when (actorAlpha g > 0.05) (drawGhost cv time g)
  forM_ (castHero cast) (dustMotes cv time)

drawHero :: Canvas s -> Int -> Actor -> Double -> ST s ()
drawHero cv time a torch = do
  shadow cv (round (actorX a)) (round (actorY a))
  when (actorFacing a /= FaceUp) (handTorch cv time a torch)
  if actorAlpha a >= 0.99
    then stamp cv id x y look
    else stampBlend cv (actorAlpha a) id x y look
  when (actorFacing a == FaceUp) (handTorch cv time a torch)
  where
    x = round (actorX a) - 4
    y = round (actorY a) - 11
    frames = case actorFacing a of
      FaceDown -> heroDown
      FaceUp   -> heroUp
      _        -> heroSide
    pose = frames !! actorStride a
    look = if actorFacing a == FaceLeft then mirrored pose else pose

drawGhost :: Canvas s -> Int -> Actor -> ST s ()
drawGhost cv time g = do
  stampBlend cv (0.75 * actorAlpha g) id x y (ghostFrames !! ((time `div` 3) `mod` 2))
  forM_ [0 .. 3 :: Int] $ \i -> do
    let life = (time + i * 3) `mod` 12
    blend cv (0.5 * actorAlpha g * (1 - fromIntegral life / 12)) (x + 1 + (i * 2) `mod` 7) (y + 9 + life `div` 3) (190, 168, 255)
  where
    hover = round (sin (fromIntegral time / 2.5) * 1.2 :: Double)
    x = round (actorX g) - 4
    y = round (actorY g) - 13 + hover

handTorch :: Canvas s -> Int -> Actor -> Double -> ST s ()
handTorch cv time a torch = do
  plot cv x y (122, 78, 50)
  plot cv x (y + 1) (78, 46, 34)
  when (torch > 0) $ do
    let tall = 1 + round (torch * 2) :: Int
        sway = if even (time `div` 2) then 0 else 1
    glow cv x (y - 1) (255, 246, 200)
    glow cv (x - 1 + sway) (y - 1) (255, 196, 86)
    forM_ [2 .. tall] $ \j ->
      glow cv (x + (if odd j then sway else 0)) (y - j) (if j == tall then (250, 132, 46) else (255, 196, 86))
  when (torch <= 0 && even (time `div` 3)) $
    blend cv 0.5 x (y - 2 - (time `div` 3) `mod` 3) (124, 116, 104)
  where
    (dx, dy) = case actorFacing a of
      FaceDown  -> (3, -4)
      FaceUp    -> (-4, -5)
      FaceLeft  -> (-4, -4)
      FaceRight -> (3, -4)
    x = round (actorX a) + dx
    y = round (actorY a) + dy

shadow :: Canvas s -> Int -> Int -> ST s ()
shadow cv x y = do
  forM_ [x - 3 .. x + 2] $ \px -> darken cv 0.55 px y
  forM_ [x - 2 .. x + 1] $ \px -> darken cv 0.7 px (y + 1)

dustMotes :: Canvas s -> Int -> Actor -> ST s ()
dustMotes cv time a = forM_ [0 .. 7 :: Int] $ \i -> do
  let period = 40 + i * 7
      phase = (time + i * 13) `mod` period
      seed = (time + i * 13) `div` period
      ox = fromIntegral (hash i seed 81 `mod` 30) - 15
      oy = fromIntegral (hash i seed 82 `mod` 20) - 16
      px = round (actorX a + ox + sin (fromIntegral phase / 6) * 1.5)
      py = round (actorY a + oy - fromIntegral phase * 0.15)
      fade = sin (fromIntegral phase / fromIntegral period * pi)
  blend cv (0.55 * fade) px py (255, 226, 170)
