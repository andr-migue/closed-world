module Screens.Backdrop (backdrop) where

import Brick
import Control.Monad (forM_, when)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Dungeon.Random (hash)
import Dungeon.Textures (brickColor, floorColor)
import Icons.Font (bigTextPixels, bigTextWidth)
import Pixels.Canvas
import Pixels.Color (scaleRGB)
import Pixels.Light
import Pixels.Terminal
import Sprites.Sprite
import Sprites.Torches
import Sprites.WallDecor (wallBanner)
import Style.Colors (stone)

backdrop :: Bool -> Int -> Widget n
backdrop titled t = Widget Greedy Greedy $ do
  ctx <- getContext
  let w = availWidth ctx
      h = availHeight ctx
  render (raw (hall titled t w h))

hall :: Bool -> Int -> Int -> Int -> V.Image
hall titled t w ch = toImage port lighting labels pixels
  where
    h = ch * 2
    wallBottom = h * 58 `div` 100
    torches = [w `div` 6, w - w `div` 6, w `div` 2 - 30, w `div` 2 + 29]
    flick salt = fromIntegral (hash (t `div` 2) salt 3 `mod` 100) / 100 - 0.5 :: Double
    torchLights =
      [ Light (fromIntegral x + 0.5) (fromIntegral (wallBottom - 18)) (40 + flick x * 4) (1.0, 0.66, 0.36) 1.1 | x <- torches ]
      ++ [ Light (fromIntegral (w `div` 2)) (fromIntegral (wallBottom - 12)) 34 (0.6, 0.45, 1.0) (0.9 + 0.15 * sin (fromIntegral t / 5)) ]
    tagline = T.pack "A puzzle game where every level is a Prolog program"
    labels = [ Label ((w - T.length tagline) `div` 2) (if 2 * bigTextWidth "CLOSED WORLD" + 10 <= w then 22 else 14) tagline (236, 226, 204) False | titled ]
    port = Viewport w h 0 0
    lighting = Lighting { ambient = 0.3, lights = torchLights, vignette = True }
    word = "CLOSED WORLD"
    scale = if 2 * bigTextWidth word + 10 <= w then 2 else 1
    tx = (w - scale * bigTextWidth word) `div` 2
    ty = 4
    glyphs = S.fromList [ (tx + scale * x + i, ty + scale * y + j) | (x, y) <- bigTextPixels word, i <- [0 .. scale - 1], j <- [0 .. scale - 1] ]
    titleInk y
      | y < ty + 2 * scale - 1 = (255, 248, 214)
      | y < ty + 3 * scale = (255, 222, 130)
      | y < ty + 5 * scale = (255, 186, 84)
      | otherwise = (212, 128, 44)
    pixels = paint port (12, 11, 19) $ \cv -> do
      forM_ [0 .. wallBottom - 1] $ \y -> forM_ [0 .. w - 1] $ \x ->
        plot cv x y (if y >= wallBottom - 2 then scaleRGB 0.6 (brickColor 0 x y) else brickColor 0 x y)
      forM_ [wallBottom .. h - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
        let shade = if y < wallBottom + 2 then 0.55 else 1
        plot cv x y (scaleRGB shade (floorColor x y))
      let gx = w `div` 2
          gw = 22
          gy = wallBottom - 30
      forM_ [gy .. wallBottom - 1] $ \y -> forM_ [gx - gw `div` 2 .. gx + gw `div` 2] $ \x -> do
        let dx = fromIntegral (x - gx) :: Double
            dy = fromIntegral (y - gy - 10) :: Double
            inside = y >= gy + 10 || dx * dx / 121 + dy * dy / 100 <= 1
            rim = y >= gy + 10 && (abs (x - gx) == gw `div` 2)
        when inside $ plot cv x y (if rim then stone 5 else (10, 8, 16))
      forM_ [0 .. 79 :: Int] $ \i -> do
        let a = fromIntegral i / 80 * 2 * pi
            ex = gx + round (cos a * 8)
            ey = wallBottom - 12 + round (sin a * 9)
            hot = (i + t) `mod` 16 < 3
        glow cv ex ey (if hot then (240, 230, 255) else (130, 92, 222))
      forM_ [0 .. 29 :: Int] $ \i -> do
        let a = fromIntegral i / 30 * 2 * pi + fromIntegral t * 0.12
            d = 2 + fromIntegral (i `mod` 6)
        glow cv (gx + round (cos a * d)) (wallBottom - 12 + round (sin a * d * 1.1)) (if odd i then (156, 236, 250) else (166, 124, 246))
      forM_ torches $ \x -> do
        stamp cv id (x - 1) (wallBottom - 15) sconce
        stampGlow cv id (x - 1) (wallBottom - 18) (sconceFlames !! ((t `div` 2 + x) `mod` 4))
        forM_ [0 .. 3 :: Int] $ \i -> do
          let life = (t + i * 6 + x) `mod` 18
              ex = x + (hash x i ((t + i * 6) `div` 18) `mod` 5) - 2
          when (life < 14) $ blend cv (1 - fromIntegral life / 15) ex (wallBottom - 19 - life) (255, 160, 70)
      forM_ [w `div` 3, w - w `div` 3 - 7] $ \x -> stamp cv id x (wallBottom - 24) wallBanner
      when titled $ do
        darkenRect cv 0.5 (tx - 4) (ty - 3) (scale * bigTextWidth word + 6) (7 * scale + 14)
        forM_ (S.toList glyphs) $ \(x, y) -> forM_ [(1, 1), (2, 2), (-1, 0), (1, 0), (0, -1), (0, 1)] $ \(dx, dy) ->
          when (S.notMember (x + dx, y + dy) glyphs) $ plot cv (x + dx) (y + dy) (if dx == 2 then (12, 9, 18) else (34, 20, 30))
        forM_ (S.toList glyphs) $ \(x, y) -> glow cv x y (titleInk y)
        forM_ [0 .. 2 :: Int] $ \i -> do
          let phase = (t + i * 23) `mod` 50
              (gx, gy) = S.elemAt (hash i ((t + i * 23) `div` 50) 9 `mod` S.size glyphs) glyphs
          when (phase < 5) $ do
            glow cv gx gy (255, 255, 255)
            forM_ [(-1, 0), (1, 0), (0, -1), (0, 1)] $ \(dx, dy) -> glow cv (gx + dx) (gy + dy) (255, 236, 170)
      forM_ [0 .. 23 :: Int] $ \i -> do
        let period = 60 + i * 5
            phase = (t + i * 17) `mod` period
            seed = (t + i * 17) `div` period
            x = hash i seed 5 `mod` max 1 w
            y = h - (phase * h `div` period)
        blend cv (0.35 * sin (fromIntegral phase / fromIntegral period * pi)) x y (255, 226, 170)
