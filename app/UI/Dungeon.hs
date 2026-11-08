module UI.Dungeon (dungeon, currentRoom) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Protocol
import UI.Theme

cellWidth :: Int
cellWidth = 22

dungeon :: Run -> Int -> Widget Name
dungeon run k = viewport DungeonView Both (vBox (concatMap row [0 .. lastRow]))
  where
    rooms   = filter ((<= k) . roomStep) (runRooms run)
    here    = currentRoom run k
    lastRow = maximum (0 : map roomRow rooms)
    lastCol = maximum (0 : map roomCol (runRooms run))
    row d   = [hBox [cell d c | c <- [0 .. lastCol]], connector run rooms lastCol d]
    cell d c = case find (\r -> roomRow r == d && roomCol r == c) rooms of
      Just r  -> roomWidget run k (Just (roomId r) == here) r
      Nothing -> vBox (replicate 4 (str (replicate cellWidth ' ')))

currentRoom :: Run -> Int -> Maybe Int
currentRoom _ 0 = Just 0
currentRoom run k = stepRoom (runSteps run !! (k - 1))

roomWidget :: Run -> Int -> Bool -> Room -> Widget Name
roomWidget run k current r =
  (if current then visible else id)
    $ padRight (Pad 2)
    $ withAttr (roomAttr current kind (roomGhost r))
    $ withBorderStyle unicodeRounded
    $ border
    $ vBox [txt (fit (roomLabel r)), txt (fit (info current kind closed))]
  where
    inner  = cellWidth - 4
    fit s  = T.justifyLeft inner ' ' (T.take inner s)
    kind   = if roomKindStep r <= k then roomKind r else T.pack "room"
    closed = length [d | d <- runDoors run, doorRoom d == roomId r,
                         doorStatus d == T.pack "closed", doorStep d <= k]

info :: Bool -> T.Text -> Int -> T.Text
info current kind closed = T.unwords (filter (not . T.null) [who, what, cuts])
  where
    who  = if current then T.pack "☺" else T.empty
    what = case T.unpack kind of
      "treasure" -> T.pack "$ treasure"
      "trap"     -> T.pack "☠ trap"
      "dead_end" -> T.pack "✗ dead end"
      "exit"     -> T.pack "∴ exit"
      "start"    -> T.pack "entrance"
      _          -> T.empty
    cuts = if closed > 0 then T.pack ("╳" ++ show closed) else T.empty

roomAttr :: Bool -> T.Text -> Bool -> AttrName
roomAttr current kind ghost
  | current              = aCurrent
  | kind == T.pack "treasure" = aTreasure
  | kind == T.pack "trap"     = aTrap
  | ghost                = aGhost
  | kind == T.pack "dead_end" = aDeadEnd
  | otherwise            = aRoom

connector :: Run -> [Room] -> Int -> Int -> Widget Name
connector run rooms lastCol d = withAttr aConnector (str (M.elems line))
  where
    width    = (lastCol + 1) * cellWidth
    center c = c * cellWidth + (cellWidth - 2) `div` 2
    children = [r | r <- rooms, roomRow r == d + 1]
    blank    = M.fromList [(i, ' ') | i <- [0 .. width - 1]]
    line     = foldl link blank children
    parentCol r = roomParent r >>= \p -> roomCol <$> find ((== p) . roomId) (runRooms run)
    link m r = case parentCol r of
      Nothing -> m
      Just pc
        | pc == roomCol r -> M.insertWith keepBranch (center pc) '│' m
        | otherwise ->
            M.insert (center (roomCol r)) '┐'
              (M.insert (center pc) '├'
                (foldl (\acc i -> M.adjust dash i acc) m [center pc + 1 .. center (roomCol r) - 1]))
    keepBranch new old = if old == '├' then old else new
    dash ' ' = '─'
    dash c   = c
