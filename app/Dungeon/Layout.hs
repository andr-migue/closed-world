module Dungeon.Layout
  ( roomW, roomH, cellW, rowH, margin
  , Layout (..)
  , Slot (..)
  , Sconce (..)
  , layout
  , origin
  , standPoint
  , barY
  , sconces
  , plaqueArea
  , chestCenter
  , trapCenter
  , portalCenter
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Prolog.Protocol

roomW, roomH, cellW, rowH, margin :: Int
roomW  = 36
roomH  = 28
cellW  = roomW + 14
rowH   = roomH + 18
margin = 14

data Layout = Layout
  { lyRooms  :: M.Map Int Room
  , lySlots  :: M.Map Int [Slot]
  , lyWidth  :: Int
  , lyHeight :: Int
  }

data Slot = Slot
  { slotDoor :: Door
  , slotX    :: Int
  , slotWide :: Bool
  }

data Sconce = Sconce
  { sconceX :: Int
  , sconceY :: Int
  }

layout :: Run -> Layout
layout run = Layout rooms slots w h
  where
    rooms = M.fromList [(roomId r, r) | r <- runRooms run]
    slots = M.mapWithKey place (M.fromListWith (flip (++)) [(doorRoom d, [d]) | d <- runDoors run])
    place rid ds =
      let ordered = sortOn doorStep ds
          n = length ordered
          usable = roomW - 8
          (ox, _) = maybe (0, 0) origin (M.lookup rid rooms)
          xAt i = ox + 4 + ((2 * i + 1) * usable) `div` (2 * n)
      in [Slot d (xAt i) (n <= 5) | (i, d) <- zip [0 ..] ordered]
    w = 2 * margin + (maximum (0 : map roomCol (runRooms run)) + 1) * cellW - (cellW - roomW)
    h = 2 * margin + (maximum (0 : map roomRow (runRooms run)) + 1) * rowH - (rowH - roomH)

origin :: Room -> (Int, Int)
origin r = (margin + roomCol r * cellW, margin + roomRow r * rowH)

standPoint :: Room -> (Double, Double)
standPoint r = let (x, y) = origin r in (fromIntegral (x + roomW `div` 2), fromIntegral (y + 22))

barY :: Room -> Int
barY r = snd (origin r) + roomH + 9

sconces :: Room -> [Sconce]
sconces r
  | roomKind r == T.pack "start" = [Sconce (x + 6) (y + 6)]
  | otherwise = [Sconce (x + 6) (y + 6), Sconce (x + roomW - 7) (y + 6)]
  where (x, y) = origin r

plaqueArea :: Room -> Int -> (Int, Int, Int)
plaqueArea r textLength = (x + (roomW - w) `div` 2, y + 4, w)
  where
    (x, y) = origin r
    w = min (roomW - 6) (textLength + 2)

chestCenter, trapCenter, portalCenter :: Room -> (Int, Int)
chestCenter r  = let (x, y) = origin r in (x + roomW - 9, y + 17)
trapCenter r   = let (x, y) = origin r in (x + roomW - 10, y + 18)
portalCenter r = let (x, y) = origin r in (x + roomW - 10, y + 17)
