module UI.Dungeon (dungeon) where

import Brick
import Data.List (find)
import qualified Graphics.Vty as V
import Protocol
import UI.Dungeon.Corridor
import UI.Dungeon.Room
import UI.Dungeon.View
import UI.Theme (Name (..))

rowHeight :: Int
rowHeight = roomHeight + 4

dungeon :: Bool -> Run -> Int -> Widget Name
dungeon follow run k = viewport DungeonView Both (focus (raw (V.vertCat (concatMap level [0 .. mvLastRow mv]))))
  where
    mv = mapView run k
    level d = [rooms d, doorsRow mv d] ++ if d < mvLastRow mv then corridorRows mv d else []
    rooms d = V.horizCat [cell d c | c <- [0 .. mvLastCol mv]]
    cell d c = case find (\r -> rvRow r == d && rvCol r == c) (mvRooms mv) of
      Just r  -> V.horizCat [roomImage r, V.charFill V.defAttr ' ' (cellWidth - roomWidth) roomHeight]
      Nothing -> V.charFill V.defAttr ' ' cellWidth roomHeight
    focus w = case find ((== Current) . rvState) (mvRooms mv) of
      Just r | follow ->
        visibleRegion (Location (rvCol r * cellWidth, rvRow r * rowHeight)) (cellWidth, rowHeight) w
      _ -> w
