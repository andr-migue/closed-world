module UI.Dungeon.Room (roomImage, roomWidth, roomHeight) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import UI.Dungeon.View
import UI.Palette
import UI.Pixel
import UI.Sprites

roomWidth, roomHeight, inner :: Int
roomWidth  = 22
roomHeight = 7
inner      = roomWidth - 2

data Look = Look
  { wall    :: RGB
  , shadow  :: RGB
  , ground  :: RGB
  , ink     :: RGB
  , shade   :: RGB -> RGB
  }

look :: RoomView -> Look
look r = case (rvState r, rvGhost r) of
  (Current, True)  -> Look wallGhost wallGhostShadow floorGhost parchment id
  (Current, False) -> Look wallGold wallGoldShadow floorWarm parchment id
  (OnPath, True)   -> Look wallGhost wallGhostShadow floorGhost parchment id
  (OnPath, False)  -> Look wallLight wallShadow floorStone parchment id
  (Abandoned, g)   ->
    let w = if g then wallGhostShadow else wallShadow
    in Look (dim w) (dim w) (dim floorStone) mist dim

roomImage :: Bool -> RoomView -> V.Image
roomImage stride r = V.vertCat ([top] ++ map framed (art ++ [label]) ++ [bottom])
  where
    l       = look r
    top     = V.charFill (fgAttr (wall l)) '▄' roomWidth 1
    bottom  = V.charFill (fgAttr (wall l)) '▀' roomWidth 1
    framed body = V.horizCat [V.char (fgAttr (shadow l)) '█', body, V.char (fgAttr (wall l)) '█']
    art     = centered l (sprites stride r)
    label   = V.text' (V.defAttr `V.withForeColor` color (ink l) `V.withBackColor` color (ground l))
                      (T.center inner ' ' (fit (rvLabel r)))
    fit s
      | T.length s <= inner - 2 = s
      | otherwise               = T.take (inner - 3) s <> T.pack "…"

sprites :: Bool -> RoomView -> [Sprite]
sprites stride r = filter (not . null) [kindSprite (rvKind r), actor]
  where
    actor
      | rvState r /= Current = []
      | rvGhost r            = ghost
      | stride               = adventurerWalk
      | otherwise            = adventurer

kindSprite :: T.Text -> Sprite
kindSprite kind = case T.unpack kind of
  "start"    -> entrance
  "treasure" -> chest
  "trap"     -> skull
  "dead_end" -> rubble
  "exit"     -> portal
  _          -> []

centered :: Look -> [Sprite] -> [V.Image]
centered l ss = map row [0 .. 3]
  where
    rendered = map (spriteRows (shade l) (ground l)) ss
    widths   = map spriteWidth ss
    used     = sum widths + 2 * max 0 (length ss - 1)
    left     = (inner - used) `div` 2
    right    = inner - used - left
    row i    = V.horizCat ([blank (ground l) left] ++ gaps [rs !! i | rs <- rendered] ++ [blank (ground l) right])
    gaps []       = []
    gaps [x]      = [x]
    gaps (x : xs) = x : blank (ground l) 2 : gaps xs

fgAttr :: RGB -> V.Attr
fgAttr c = V.defAttr `V.withForeColor` color c
