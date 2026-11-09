module Dungeon.Effects
  ( roomEffects
  , stepEffects
  , doorsChangedAt
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dungeon.Layout
import Dungeon.Random (hash)
import Dungeon.Timeline
import Pixels.Canvas
import Prolog.Protocol

roomEffects :: Canvas s -> Int -> Room -> RoomView -> ST s ()
roomEffects cv time r v = case T.unpack (rvKind v) of
  "exit"     -> portalSwirl cv time r
  "treasure" -> treasureGlints cv time r
  _          -> when (rvState v /= Abandoned) (torchEmbers cv time r)

portalSwirl :: Canvas s -> Int -> Room -> ST s ()
portalSwirl cv time r = do
  forM_ [0 .. 59 :: Int] $ \i -> do
    let a = fromIntegral i / 60 * 2 * pi
        hot = (i + time) `mod` 12 < 3
    glow cv (cx + round (cos a * 7)) (cy + round (sin a * 5)) (if hot then (240, 230, 255) else (150, 112, 236))
  forM_ [0 .. 23 :: Int] $ \i -> do
    let a = fromIntegral i / 24 * 2 * pi + fromIntegral time * 0.15
        d = 2 + fromIntegral (i `mod` 4)
    glow cv (cx + round (cos a * d)) (cy + round (sin a * d * 0.7)) (if odd i then (130, 92, 222) else (156, 236, 250))
  where (cx, cy) = portalCenter r

treasureGlints :: Canvas s -> Int -> Room -> ST s ()
treasureGlints cv time r = forM_ [0 .. 2 :: Int] $ \i -> do
  let cycleNo = (time + i * 7) `div` 21
  when ((time + i * 7) `mod` 21 < 4) $
    sparkle cv (cx - 4 + hash (roomId r) i cycleNo `mod` 9) (cy - 6 + hash i (roomId r) cycleNo `mod` 4)
  where (cx, cy) = chestCenter r

torchEmbers :: Canvas s -> Int -> Room -> ST s ()
torchEmbers cv time r = forM_ (sconces r) $ \s -> forM_ [0 .. 1 :: Int] $ \i -> do
  let life = (time + i * 5 + sconceX s) `mod` 10
      ex = sconceX s + (hash (sconceX s) i ((time + i * 5) `div` 10) `mod` 3) - 1
  when (life < 6) $ blend cv (1 - fromIntegral life / 7) ex (sconceY s - 1 - life) (255, 160, 70)

stepEffects :: Canvas s -> Layout -> Run -> Int -> Double -> Int -> ST s ()
stepEffects cv ly run k p time = case T.unpack . stepEvent <$> eventAt run k of
  Just "cut" -> forM_ (doorsChangedAt ly k "closed") $ \(x, y) ->
    forM_ [0 .. 5 :: Int] $ \i -> do
      let a = fromIntegral i * 1.05 + p * 3
          d = 2 + p * 6
      when (p < 0.9) $ glow cv (x + round (cos a * d)) (y - 1 + round (sin a * d * 0.6) - 2) (255, 110, 120)
  Just "success" -> forM_ (currentRoom run k >>= \i -> M.lookup i (lyRooms ly)) $ \r -> do
    let (cx, cy) = chestCenter r
    forM_ [0 .. 9 :: Int] $ \i -> do
      let a = fromIntegral i * 0.63 + fromIntegral time * 0.05
          d = 4 + p * 12
      sparkle cv (cx + round (cos a * d)) (cy - 2 + round (sin a * d * 0.6) - round (p * 4))
  _ -> pure ()

doorsChangedAt :: Layout -> Int -> String -> [(Int, Int)]
doorsChangedAt ly k status =
  [ (slotX s, snd (origin r) + 26)
  | (rid, ss) <- M.toList (lySlots ly), Just r <- [M.lookup rid (lyRooms ly)]
  , s <- ss, doorStep (slotDoor s) == k, doorStatus (slotDoor s) == T.pack status ]

sparkle :: Canvas s -> Int -> Int -> ST s ()
sparkle cv x y = do
  glow cv x y (255, 250, 220)
  forM_ [(-1, 0), (1, 0), (0, -1), (0, 1)] $ \(dx, dy) -> glow cv (x + dx) (y + dy) (255, 210, 110)
