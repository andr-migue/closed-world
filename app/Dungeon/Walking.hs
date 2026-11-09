module Dungeon.Walking
  ( Facing (..)
  , Actor (..)
  , Route
  , ancestry
  , route
  , bump
  , walk
  ) where

import Data.List (find)
import qualified Data.Map.Strict as M
import Dungeon.Layout
import Prolog.Protocol

data Facing = FaceDown | FaceUp | FaceLeft | FaceRight
  deriving Eq

data Actor = Actor
  { actorX      :: Double
  , actorY      :: Double
  , actorFacing :: Facing
  , actorStride :: Int
  , actorAlpha  :: Double
  }

type Route = [(Double, Double)]

ancestry :: Layout -> Int -> [Room]
ancestry ly i = case M.lookup i (lyRooms ly) of
  Nothing -> []
  Just r  -> r : maybe [] (ancestry ly) (roomParent r)

route :: Layout -> Int -> Int -> Route
route ly from to
  | from == to = maybe [] (\r -> [standPoint r]) (M.lookup to (lyRooms ly))
  | otherwise  = dedupe (concat (ups ++ downs))
  where
    upChain   = ancestry ly from
    downChain = ancestry ly to
    common    = find (\r -> roomId r `elem` map roomId downChain) upChain
    stop      = maybe (-1) roomId common
    climb     = takeWhile ((/= stop) . roomId) upChain
    sink      = reverse (takeWhile ((/= stop) . roomId) downChain)
    parentOf r = roomParent r >>= \p -> M.lookup p (lyRooms ly)
    ups   = [reverse (descend ly p c) | c <- climb, Just p <- [parentOf c]]
    downs = [descend ly p c | c <- sink, Just p <- [parentOf c]]

descend :: Layout -> Room -> Room -> Route
descend ly parent child =
  [ standPoint parent, (dx, py + 22), (dx, by), (cx, by), (cx, cy + 13), standPoint child ]
  where
    dx = maybe (fst (standPoint parent)) fromIntegral (doorTo ly parent child)
    by = fromIntegral (barY parent + 1)
    py = fromIntegral (snd (origin parent))
    (cx0, cy0) = origin child
    cx = fromIntegral (cx0 + roomW `div` 2)
    cy = fromIntegral cy0

doorTo :: Layout -> Room -> Room -> Maybe Int
doorTo ly parent child =
  slotX <$> find ((== Just (roomId child)) . doorTarget . slotDoor) (M.findWithDefault [] (roomId parent) (lySlots ly))

bump :: Layout -> Room -> Door -> Route
bump ly r d = case find ((== doorStep d) . doorStep . slotDoor) (M.findWithDefault [] (roomId r) (lySlots ly)) of
  Just s  -> let (_, y) = origin r in [standPoint r, (fromIntegral (slotX s), fromIntegral (y + 23)), standPoint r]
  Nothing -> [standPoint r]

dedupe :: Route -> Route
dedupe (a : b : rest)
  | a == b    = dedupe (b : rest)
  | otherwise = a : dedupe (b : rest)
dedupe xs = xs

walk :: Route -> Double -> Actor
walk []  _ = Actor 0 0 FaceDown 0 1
walk [p] _ = Actor (fst p) (snd p) FaceDown 0 1
walk pts t = go pts (t * total)
  where
    segments = zip pts (tail pts)
    total = sum (map len segments)
    len ((x1, y1), (x2, y2)) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    moving = t > 0 && t < 1
    go (a : b : rest) s
      | s <= len (a, b) || null rest =
          let l = len (a, b)
              f = if l == 0 then 1 else min 1 (s / l)
              x = fst a + (fst b - fst a) * f
              y = snd a + (snd b - snd a) * f
              stride = if moving then 1 + (floor ((t * total) / 3) `mod` 2) else 0
          in Actor x y (facing a b) stride 1
      | otherwise = go (b : rest) (s - len (a, b))
    go [p] _ = Actor (fst p) (snd p) FaceDown 0 1
    go [] _ = Actor 0 0 FaceDown 0 1
    facing a b
      | not moving = FaceDown
      | abs (fst b - fst a) > abs (snd b - snd a) = if fst b > fst a then FaceRight else FaceLeft
      | snd b < snd a = FaceUp
      | otherwise = FaceDown
