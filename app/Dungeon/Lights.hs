module Dungeon.Lights (sceneLights) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dungeon.Cast
import Dungeon.Effects (doorsChangedAt)
import Dungeon.Layout
import Dungeon.Random (hash)
import Dungeon.Timeline
import Dungeon.Walking (Actor (..))
import Pixels.Light (Light (..))
import Prolog.Protocol

sceneLights :: Layout -> Run -> Int -> Double -> Int -> Double -> Cast -> [(Room, RoomView)] -> [Light]
sceneLights ly run k p time torch cast rooms =
  heroTorch ++ ghostGlow ++ concatMap (uncurry (roomLights time)) rooms ++ stepLights ly run k p
  where
    heroTorch = case castHero cast of
      Just a | torch > 0 ->
        [Light (actorX a + 3) (actorY a - 8) (16 + 30 * torch + flicker time 0 * 1.5) (1.0, 0.74, 0.46) (1.3 * actorAlpha a)]
      _ -> []
    ghostGlow = case castGhost cast of
      Just g -> [Light (actorX g) (actorY g - 6) 16 (0.62, 0.5, 1.0) (0.9 * actorAlpha g)]
      Nothing -> []

roomLights :: Int -> Room -> RoomView -> [Light]
roomLights time r v =
  [ Light (fromIntegral (sconceX s) + 0.5) (fromIntegral (sconceY s) + 1) (17 + flicker time (sconceX s) * 2) (1.0, 0.66, 0.36) 0.75
  | rvState v /= Abandoned, s <- sconces r ]
  ++ case T.unpack (rvKind v) of
       "treasure" -> [at (chestCenter r) 26 (1.0, 0.84, 0.4) 1.1]
       "trap"     -> [at (trapCenter r) 22 (1.0, 0.25, 0.3) (0.8 + 0.3 * sin (fromIntegral time / 3))]
       "exit"     -> [at (portalCenter r) 22 (0.6, 0.45, 1.0) 1.0]
       _          -> []

stepLights :: Layout -> Run -> Int -> Double -> [Light]
stepLights ly run k p = case T.unpack . stepEvent <$> eventAt run k of
  Just "cut"       -> [ at d 9 (1.0, 0.2, 0.3) (1.6 * (1 - p)) | d <- doorsChangedAt ly k "closed" ]
  Just "mismatch"  -> [ at d 7 (1.0, 0.3, 0.3) (1.4 * pulse) | d <- doorsChangedAt ly k "shut" ]
  Just "success"   -> [ at (chestCenter r) (8 + 40 * p) (1.0, 0.86, 0.45) (1.5 * (1 - p * 0.5)) | Just r <- [here] ]
  Just "trap"      -> [ at (trapCenter r) (10 + 30 * p) (1.0, 0.15, 0.2) 1.6 | Just r <- [here] ]
  Just "not_start" -> [ at (fst (origin r) + roomW `div` 2, snd (origin r) + 18) (6 + 20 * p) (0.6, 0.45, 1.0) (1.4 * (1 - p)) | Just r <- [here] ]
  _ -> []
  where
    here = currentRoom run k >>= \i -> M.lookup i (lyRooms ly)
    pulse = max 0 (1 - abs (p - 0.45) * 4)

at :: (Int, Int) -> Double -> (Double, Double, Double) -> Double -> Light
at (x, y) = Light (fromIntegral x) (fromIntegral y)

flicker :: Int -> Int -> Double
flicker time salt = fromIntegral (hash (time `div` 2) salt 3 `mod` 100) / 100 - 0.5
