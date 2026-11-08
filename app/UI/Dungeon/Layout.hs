module UI.Dungeon.Layout
  ( roomW, roomH, cellW, rowH, margin
  , Layout (..)
  , Slot (..)
  , layout
  , origin
  , standPoint
  , barY
  , Facing (..)
  , Actor (..)
  , Route
  , route
  , bump
  , walk
  , ancestry
  ) where

import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Protocol

roomW, roomH, cellW, rowH, margin :: Int
roomW  = 36
roomH  = 28
cellW  = roomW + 14
rowH   = roomH + 18
margin = 14

data Slot = Slot
  { slotDoor :: Door
  , slotX    :: Int
  , slotWide :: Bool
  }

data Layout = Layout
  { lyRooms  :: M.Map Int Room
  , lySlots  :: M.Map Int [Slot]
  , lyWidth  :: Int
  , lyHeight :: Int
  }

layout :: Run -> Layout
layout run = Layout rooms slots w h
  where
    rooms = M.fromList [(roomId r, r) | r <- runRooms run]
    slots = M.mapWithKey place (M.fromListWith (flip (++)) [(doorRoom d, [d]) | d <- runDoors run])
    place rid ds =
      let ordered = sortOn doorStep ds
          n = length ordered
          span' = roomW - 8
          (ox, _) = maybe (0, 0) origin (M.lookup rid rooms)
          xAt i = ox + 4 + ((2 * i + 1) * span') `div` (2 * n)
      in [Slot d (xAt i) (n <= 5) | (i, d) <- zip [0 ..] ordered]
    w = 2 * margin + (maximum (0 : map roomCol (runRooms run)) + 1) * cellW - (cellW - roomW)
    h = 2 * margin + (maximum (0 : map roomRow (runRooms run)) + 1) * rowH - (rowH - roomH)

origin :: Room -> (Int, Int)
origin r = (margin + roomCol r * cellW, margin + roomRow r * rowH)

standPoint :: Room -> (Double, Double)
standPoint r = let (x, y) = origin r in (fromIntegral (x + roomW `div` 2), fromIntegral (y + 22))

barY :: Room -> Int
barY r = snd (origin r) + roomH + 9

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

doorTo :: Layout -> Room -> Room -> Maybe Int
doorTo ly parent child =
  slotX <$> find ((== Just (roomId child)) . doorTarget . slotDoor) (M.findWithDefault [] (roomId parent) (lySlots ly))

descend :: Layout -> Room -> Room -> Route
descend ly parent child =
  [ standPoint parent, (dx, py + 22), (dx, by), (cx, by), (cx, cy + 13), standPoint child ]
  where
    dx = maybe (fst (standPoint parent)) fromIntegral (doorTo ly parent child)
    by = fromIntegral (barY parent + 1)
    (_, py0) = origin parent
    py = fromIntegral py0
    (cx0, cy0) = origin child
    cx = fromIntegral (cx0 + roomW `div` 2)
    cy = fromIntegral cy0

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
    segs  = zip pts (tail pts)
    total = sum (map len segs)
    len ((x1, y1), (x2, y2)) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    go (a : b : rest) s
      | s <= len (a, b) || null rest =
          let l = len (a, b)
              f = if l == 0 then 1 else min 1 (s / l)
              x = fst a + (fst b - fst a) * f
              y = snd a + (snd b - snd a) * f
              moving = t > 0 && t < 1
              stride = if moving then 1 + (floor ((t * total) / 3) `mod` 2) else 0
              facing
                | not moving = FaceDown
                | abs (fst b - fst a) > abs (snd b - snd a) = if fst b > fst a then FaceRight else FaceLeft
                | snd b < snd a = FaceUp
                | otherwise = FaceDown
          in Actor x y facing stride 1
      | otherwise = go (b : rest) (s - len (a, b))
    go [p] _ = Actor (fst p) (snd p) FaceDown 0 1
    go [] _ = Actor 0 0 FaceDown 0 1
