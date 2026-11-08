module UI.Dungeon.Corridor (cellWidth, center, doorsRow, corridorRows) where

import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Protocol
import UI.Dungeon.Room (roomWidth)
import UI.Dungeon.View
import UI.Palette

cellWidth :: Int
cellWidth = roomWidth + 4

center :: Int -> Int
center c = c * cellWidth + roomWidth `div` 2 - 1

doorsRow :: MapView -> Int -> V.Image
doorsRow mv d = V.horizCat [cell c | c <- [0 .. mvLastCol mv]]
  where
    cell c = case find (\r -> rvRow r == d && rvCol r == c) (mvRooms mv) of
      Just r  -> fitWidth (V.horizCat (concatMap (token mv r) (rvDoors r)))
      Nothing -> V.charFill V.defAttr ' ' cellWidth 1
    fitWidth img = V.cropRight cellWidth (V.horizCat [img, V.charFill V.defAttr ' ' cellWidth 1])

token :: MapView -> RoomView -> Door -> [V.Image]
token mv r door = [V.string (fg ink) (icon : clause), V.char V.defAttr ' ']
  where
    clause = maybe "" show (doorClause door)
    faded  = rvState r == Abandoned
    shown c = if faded then dim c else c
    (icon, ink) = case T.unpack (doorStatus door) of
      "open"   -> ('▾', shown (if lit (doorTarget door) then gold else steelDeep))
      "shut"   -> ('×', shown mist)
      "closed" -> ('▣', shown crimson)
      "ghost"  -> ('✦', shown arcane)
      _        -> ('?', mist)
    lit target = case target >>= \t -> find ((== t) . rvId) (mvRooms mv) of
      Just child -> rvState child /= Abandoned
      Nothing    -> False

corridorRows :: MapView -> Int -> [V.Image]
corridorRows mv d = map render [stubs, bars, drops]
  where
    children = [c | c <- mvRooms mv, rvRow c == d + 1]
    links    = [(pc, c) | c <- children, Just pc <- [parentCol c]]
    parentCol c = rvParent c >>= \p -> rvCol <$> find ((== p) . rvId) (mvRooms mv)
    width    = (mvLastCol mv + 1) * cellWidth
    render m = V.horizCat [maybe (V.char V.defAttr ' ') draw (M.lookup i m) | i <- [0 .. width - 1]]
    draw (ch, ink) = V.char (fg ink) ch
    stubs = M.fromListWith brighter [(center pc, ('┃', style c)) | (pc, c) <- links]
    drops = M.fromListWith brighter [(center (rvCol c), ('┃', style c)) | (_, c) <- links]
    bars  = foldl bar M.empty (sortOn (\(pc, c) -> (pc, rvCol c)) links)
    bar m (pc, c)
      | pc == rvCol c = M.insertWith join (center pc) ('┃', style c) m
      | otherwise =
          let from = center pc
              to   = center (rvCol c)
              run  = M.fromList [(i, ('━', style c)) | i <- [from + 1 .. to - 1]]
              m1   = M.unionWith join m run
              m2   = M.insertWith join from ('┗', style c) m1
          in M.insertWith join to ('┓', style c) m2

join :: (Char, RGB) -> (Char, RGB) -> (Char, RGB)
join (new, a) (old, b) = (glyph new old, brightest a b)
  where
    glyph '┗' '┃' = '┣'
    glyph '┃' '┗' = '┣'
    glyph '┗' '┣' = '┣'
    glyph '┗' '┗' = '┗'
    glyph '━' '┓' = '┳'
    glyph '┓' '━' = '┳'
    glyph '━' '━' = '━'
    glyph '┓' '┓' = '┓'
    glyph '━' '┃' = '╋'
    glyph '┃' '━' = '╋'
    glyph n _     = n

brighter :: (Char, RGB) -> (Char, RGB) -> (Char, RGB)
brighter (c, a) (_, b) = (c, brightest a b)

brightest :: RGB -> RGB -> RGB
brightest a b
  | rank a >= rank b = a
  | otherwise        = b
  where
    rank x
      | x == gold   = 3 :: Int
      | x == arcane = 2
      | otherwise   = 1

style :: RoomView -> RGB
style c = case (rvState c, rvGhost c) of
  (Abandoned, _) -> dim wallShadow
  (_, True)      -> arcane
  _              -> gold

fg :: RGB -> V.Attr
fg c = V.defAttr `V.withForeColor` color c
