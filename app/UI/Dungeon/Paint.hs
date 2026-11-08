module UI.Dungeon.Paint
  ( Look (..)
  , Corridor (..)
  , hash
  , voidRock
  , roomShell
  , corridorWalls
  , corridorFloor
  , roomDetails
  , doorway
  , memoryTone
  , ghostTone
  , plain
  , Sconce (..)
  , sconces
  , plaqueLabel
  , portalCenter
  , chestCenter
  , trapCenter
  , floorColor
  , brickColor
  ) where

import Control.Monad (forM_, when, unless)
import Control.Monad.ST
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.Text as T
import Protocol
import UI.Art
import UI.Canvas
import UI.Dungeon.Layout
import UI.Palette (RGB)

data Look = Look
  { lookTone  :: Tone
  , lookLit   :: Bool
  , lookGhost :: Bool
  , lookHere  :: Bool
  }

data Corridor = Corridor
  { corDoorX  :: Int
  , corChildX :: Int
  , corTop    :: Int
  , corBar    :: Int
  , corBottom :: Int
  , corLook   :: Look
  }

data Sconce = Sconce
  { sconceX :: Int
  , sconceY :: Int
  }

hash :: Int -> Int -> Int -> Int
hash a b c = h3 .&. 0x7fffffff
  where
    h0 = (a * 73856093) `xor` (b * 19349663) `xor` (c * 83492791)
    h1 = (h0 `xor` (h0 `shiftR` 13)) * 1274126177
    h2 = h1 `xor` (h1 `shiftR` 16)
    h3 = h2 * 668265263 `xor` (h2 `shiftR` 11)

chance :: Int -> Int -> Int -> Int -> Bool
chance n a b c = hash a b c `mod` 100 < n

plain :: Tone
plain = id

memoryTone :: Tone
memoryTone c = scaleRGB 0.5 (mixRGB 0.6 c (l * 8 `div` 10, l * 85 `div` 100, min 255 (l * 118 `div` 100)))
  where l = luma c

ghostTone :: Tone
ghostTone c = mixRGB 0.5 c (l * 72 `div` 100, l * 58 `div` 100, min 255 (l * 140 `div` 100 + 18))
  where l = luma c

voidRock :: Canvas s -> Int -> Int -> Int -> Int -> ST s ()
voidRock cv x0 y0 w h = forM_ [y0 .. y0 + h - 1] $ \y -> forM_ [x0 .. x0 + w - 1] $ \x -> do
  let v = hash x y 7 `mod` 100
      c | v < 4     = (24, 22, 34)
        | v < 14    = (17, 16, 26)
        | otherwise = (12, 11, 19)
  plot cv x y c

floorColor :: Int -> Int -> RGB
floorColor x y
  | lx == 0 || ly == 0 = (50, 43, 52)
  | crack && (lx == ly + 1 || lx == ly + 2) && ly < 3 = (60, 52, 60)
  | moss && lx == 1 && ly == 3 = (58, 112, 74)
  | lx == 1 || ly == 1 = lift 9 base
  | otherwise = base
  where
    ty = y `div` 4
    shift = if odd ty then 3 else 0
    tx = (x + shift) `div` 6
    lx = (x + shift) `mod` 6
    ly = y `mod` 4
    v = hash tx ty 11 `mod` 7
    base = case v of
      0 -> (80, 70, 74)
      1 -> (96, 85, 86)
      2 -> (88, 78, 80)
      3 -> (104, 93, 92)
      4 -> (92, 82, 84)
      5 -> (84, 74, 78)
      _ -> (98, 88, 90)
    crack = hash tx ty 13 `mod` 11 == 0
    moss  = hash tx ty 17 `mod` 9 == 0

cobbleColor :: Int -> Int -> RGB
cobbleColor x y
  | lx == 0 || ly == 0 = (44, 38, 46)
  | lx == 1 && ly == 1 = lift 10 base
  | otherwise = base
  where
    ty = y `div` 3
    shift = if odd ty then 2 else 0
    tx = (x + shift) `div` 4
    lx = (x + shift) `mod` 4
    ly = y `mod` 3
    base = case hash tx ty 23 `mod` 4 of
      0 -> (70, 62, 68)
      1 -> (80, 71, 76)
      2 -> (74, 66, 72)
      _ -> (86, 77, 80)

lift :: Int -> RGB -> RGB
lift n (r, g, b) = (r + n, g + n, b + n)

brickColor :: Int -> Int -> Int -> RGB
brickColor top x y
  | ly == 2 = stoneTone 1
  | lx == 0 = stoneTone 1
  | ly == 0 = if hash bx by 5 `mod` 3 == 0 then stoneTone 5 else stoneTone 4
  | otherwise = case hash bx by 3 `mod` 5 of
      0 -> stoneTone 2
      1 -> (66, 70, 98)
      _ -> stoneTone 3
  where
    ry = y - top
    by = ry `div` 3
    shift = if odd by then 3 else 0
    bx = (x + shift) `div` 6
    lx = (x + shift) `mod` 6
    ly = ry `mod` 3

roomShell :: Canvas s -> Look -> Room -> ST s ()
roomShell cv lk r = when (inView cv x y roomW roomH) $ do
  let t = lookTone lk
      p px py c = plot cv px py (t c)
  forM_ [y + 11 .. y + 25] $ \py -> forM_ [x + 2 .. x + roomW - 3] $ \px -> do
    let c = floorColor px py
        shadow
          | py == y + 11 = 0.55
          | py == y + 12 = 0.78
          | px == x + 2 || px == x + roomW - 3 = 0.8
          | otherwise = 1
    p px py (scaleRGB shadow c)
  forM_ [y + 3 .. y + 10] $ \py -> forM_ [x + 2 .. x + roomW - 3] $ \px ->
    p px py (if py == y + 10 then scaleRGB 0.7 (brickColor (y + 3) px py) else brickColor (y + 3) px py)
  forM_ [x .. x + roomW - 1] $ \px -> do
    p px y (26, 20, 36)
    p px (y + 1) (if hash px y 9 `mod` 5 == 0 then stoneTone 3 else stoneTone 4)
    p px (y + 2) (stoneTone 3)
    p px (y + roomH - 2) (stoneTone 3)
    p px (y + roomH - 1) (26, 20, 36)
  forM_ [y .. y + roomH - 1] $ \py -> do
    p x py (26, 20, 36)
    p (x + 1) py (if py < y + 3 then stoneTone 4 else stoneTone 3)
    p (x + roomW - 2) py (if py < y + 3 then stoneTone 4 else stoneTone 3)
    p (x + roomW - 1) py (26, 20, 36)
  where (x, y) = origin r

corridorWalls :: Canvas s -> Corridor -> ST s ()
corridorWalls cv c = do
  let t = lookTone (corLook c)
      p px py col = plot cv px py (t col)
      b = corBar c
      lo = min (corDoorX c) (corChildX c) - 4
      hi = max (corDoorX c) (corChildX c) + 3
  forM_ [lo .. hi] $ \px -> do
    p px (b - 6) (26, 20, 36)
    p px (b - 5) (stoneTone 4)
    p px (b - 4) (brickColor (b - 4) px (b - 4))
    p px (b - 3) (brickColor (b - 4) px (b - 3))
    p px (b + 2) (stoneTone 3)
    p px (b + 3) (26, 20, 36)
  let column cx y1 y2 = forM_ [y1 .. y2] $ \py -> do
        p (cx - 4) py (26, 20, 36)
        p (cx - 3) py (stoneTone 3)
        p (cx + 2) py (stoneTone 3)
        p (cx + 3) py (26, 20, 36)
  column (corDoorX c) (corTop c) (b - 6)
  column (corChildX c) (b + 3) (corBottom c - 1)

corridorFloor :: Canvas s -> Corridor -> ST s ()
corridorFloor cv c = do
  let t = lookTone (corLook c)
      b = corBar c
      lo = min (corDoorX c) (corChildX c) - 2
      hi = max (corDoorX c) (corChildX c) + 1
      fl px py shade = plot cv px py (t (scaleRGB shade (cobbleColor px py)))
  forM_ [corTop c - 2 .. b + 1] $ \py -> forM_ [corDoorX c - 2 .. corDoorX c + 1] $ \px ->
    fl px py (if py < corTop c then 0.8 else 1)
  forM_ [b - 2 .. b + 1] $ \py -> forM_ [lo .. hi] $ \px ->
    fl px py (if py == b - 2 then 0.7 else 1)
  forM_ [b - 2 .. corBottom c + 10] $ \py -> forM_ [corChildX c - 2 .. corChildX c + 1] $ \px -> do
    let depth
          | py < corBottom c = 1
          | otherwise = 0.5 + 0.05 * fromIntegral (py - corBottom c)
    fl px py depth
  forM_ [corBottom c .. corBottom c + 10] $ \py -> do
    plot cv (corChildX c - 3) py (t (stoneTone (if py < corBottom c + 3 then 5 else 4)))
    plot cv (corChildX c + 2) py (t (stoneTone (if py < corBottom c + 3 then 4 else 2)))

doorway :: Canvas s -> Look -> Room -> Slot -> ST s ()
doorway cv lk r s = do
  let (_, y) = origin r
      x = slotX s
      t = lookTone lk
      d = slotDoor s
      wide = slotWide s
  case T.unpack (doorStatus d) of
    status | status == "open" || status == "ghost" -> do
      forM_ [y + 24 .. y + roomH - 1] $ \py -> forM_ [x - 2 .. x + 1] $ \px ->
        plot cv px py (t (cobbleColor px py))
      let jamb = if status == "ghost" then (150, 112, 236) else stoneTone 4
      forM_ [y + 24 .. y + roomH - 1] $ \py -> do
        plot cv (x - 3) py (t jamb)
        plot cv (x + 2) py (t (if status == "ghost" then (96, 70, 170) else stoneTone 2))
      when (status == "ghost") $ forM_ [x - 2 .. x + 1] $ \px ->
        glow cv px (y + roomH - 2) (t (170, 140, 255))
    "shut" -> stamp cv t (if wide then x - 2 else x - 1) (y + 24) (if wide then doorShut else doorShutNarrow)
    "closed" -> stampGlow cv (dimIf lk) (if wide then x - 2 else x - 1) (y + 24) (if wide then doorSealed else doorSealedNarrow)
    _ -> pure ()
  where
    dimIf l = if lookLit l then id else scaleRGB 0.55 . lookTone l

plaqueLabel :: Room -> Int -> (Int, Int, Int)
plaqueLabel r n = (x + (roomW - w) `div` 2, y + 4, w)
  where
    (x, y) = origin r
    w = min (roomW - 6) (n + 2)

sconces :: Room -> [Sconce]
sconces r
  | roomKind r == T.pack "start" = [Sconce (x + 6) (y + 6)]
  | otherwise = [Sconce (x + 6) (y + 6), Sconce (x + roomW - 7) (y + 6)]
  where (x, y) = origin r

portalCenter, chestCenter, trapCenter :: Room -> (Int, Int)
portalCenter r = let (x, y) = origin r in (x + roomW - 10, y + 17)
chestCenter r  = let (x, y) = origin r in (x + roomW - 9, y + 17)
trapCenter r   = let (x, y) = origin r in (x + roomW - 10, y + 18)

special :: T.Text -> Bool
special k = k `elem` map T.pack ["treasure", "trap", "dead_end", "exit"]

roomDetails :: Canvas s -> Int -> Look -> Room -> T.Text -> Int -> ST s ()
roomDetails cv time lk r kind labelLen = when (inView cv x y roomW roomH) $ do
  let t = lookTone lk
      rid = roomId r
      (px0, py0, pw) = plaqueLabel r labelLen
      clear a b = a + b <= px0 - 1 || a >= px0 + pw + 1
  when (labelLen > 0) $ do
    forM_ [px0 .. px0 + pw - 1] $ \px -> do
      glow cv px py0 (t (40, 30, 30))
      glow cv px (py0 + 1) (t (30, 22, 24))
    glow cv px0 py0 (t (232, 168, 60))
    glow cv (px0 + pw - 1) py0 (t (232, 168, 60))
    glow cv px0 (py0 + 1) (t (168, 100, 36))
    glow cv (px0 + pw - 1) (py0 + 1) (t (168, 100, 36))
  forM_ (sconces r) $ \sc -> do
    stamp cv t (sconceX sc - 1) (sconceY sc + 3) sconce
    if lookLit lk
      then stampGlow cv id (sconceX sc - 1) (sconceY sc)
             (sconceFlames !! ((time `div` 2 + hash (sconceX sc) rid 1) `mod` 4))
      else stamp cv t (sconceX sc - 1) (sconceY sc) sconceCold
  when (chance 55 rid 2 0 && clear (x + 2) 6) $ stampBlend cv 0.5 t (x + 2) (y + 3) cobweb
  when (chance 45 rid 3 0 && clear (x + roomW - 8) 6) $ stampBlend cv 0.5 t (x + roomW - 8) (y + 3) (mirrored cobweb)
  forM_ [(x + 10, 4), (x + roomW - 17, 5)] $ \(bx, salt) ->
    when (chance 40 rid salt 0 && clear bx 7 && kind /= T.pack "start") $ stamp cv t bx (y + 3) banner
  forM_ [0 .. 2 :: Int] $ \i -> do
    let cx = x + 3 + hash rid i 31 `mod` (roomW - 6)
    when (chance 50 rid i 32 && clear cx 1) $
      forM_ [0 .. 3] $ \k -> plot cv (cx + (k `mod` 2)) (y + 4 + k) (t (stoneTone 1))
  when (chance 35 rid 6 0) $ do
    let cx = x + 12 + hash rid 6 1 `mod` 12
    when (clear cx 1) $ forM_ [0 .. 4] $ \k -> plot cv cx (y + 3 + k) (t (if even k then stoneTone 5 else (94, 102, 130)))
  forM_ [x + 2 .. x + roomW - 3] $ \mx -> do
    when (hash mx rid 41 `mod` 7 == 0) $ plot cv mx (y + 10) (t (58, 142, 92))
    when (hash mx rid 42 `mod` 13 == 0) $ plot cv mx (y + 9) (t (30, 86, 60))
    when (hash mx rid 43 `mod` 17 == 0) $ plot cv mx (y + 11) (t (58, 142, 92))
  let props = [barrel, crate, urn, bones, skullSmall, mushrooms, crate, barrel]
      slotsAt =
        [ (x + 3, y + 12, 0), (x + roomW - 3, y + 12, 1)
        , (x + 3, y + 18, 2), (x + roomW - 3, y + 18, 3) ]
  forM_ slotsAt $ \(sx, sy, salt) ->
    when (not (odd salt && special (roomKind r)) && chance (if salt < 2 then 70 else 40) rid salt 50) $ do
      let a = props !! (hash rid salt 51 `mod` length props)
          ax = if odd salt then sx - artW a else sx
          isGlow = artW a == 6 && artH a == 4 && hash rid salt 51 `mod` length props == 5
      if isGlow then stampGlow cv (if lookLit lk then id else scaleRGB 0.6) ax sy a else stamp cv t ax sy a
  when (chance 40 rid 9 0) $ do
    let wx = x + 9 + hash rid 9 1 `mod` 16
        wy = y + 14 + hash rid 9 2 `mod` 7
    forM_ [(-2) .. 3] $ \k -> forM_ [0, 1] $ \j ->
      unless (j == 1 && (k == -2 || k == 3)) $
        plot cv (wx + k) (wy + j) (t (if j == 0 && k == 0 then (150, 200, 230) else if j == 0 then (74, 124, 168) else (36, 64, 104)))
  case T.unpack kind of
    "start" -> do
      forM_ [0 .. 3] $ \s -> forM_ [x + roomW - 13 .. x + roomW - 5] $ \px -> do
        plot cv px (y + 3 + s * 2) (t (scaleRGB (0.35 + 0.15 * fromIntegral s) (stoneTone 5)))
        plot cv px (y + 4 + s * 2) (t (scaleRGB (0.3 + 0.12 * fromIntegral s) (stoneTone 2)))
      forM_ [y + 3 .. y + 10] $ \py -> do
        plot cv (x + roomW - 14) py (t (stoneTone 5))
        plot cv (x + roomW - 4) py (t (26, 20, 36))
      forM_ [y + 12 .. y + 24] $ \py -> forM_ [x + 13 .. x + 22] $ \px -> do
        let edge = px == x + 13 || px == x + 22
            trim = px == x + 14 || px == x + 21
            c | edge = (104, 22, 42)
              | trim = (232, 168, 60)
              | (px + py) `mod` 4 == 0 = (150, 34, 56)
              | otherwise = (182, 42, 64)
        plot cv px py (t c)
    "treasure" -> do
      let (cx, cy) = chestCenter r
      stamp cv t (cx - 5) (cy - 4) chestOpen
      forM_ [0 .. 5 :: Int] $ \i -> do
        let gx = cx - 8 + hash rid i 61 `mod` 16
            gy = cy + 4 + hash rid i 62 `mod` 4
        glow cv gx gy (255, 222, 130)
    "trap" -> do
      let (cx, cy) = trapCenter r
      forM_ [cy - 3 .. cy + 3] $ \py -> forM_ [cx - 6 .. cx + 5] $ \px -> do
        let edge = py == cy - 3 || px == cx - 6 || px == cx + 5
        plot cv px py (t (if edge then (26, 20, 36) else (14, 10, 20)))
      forM_ [cx - 4, cx - 1, cx + 2] $ \sx -> forM_ [cy - 1, cy + 2] $ \sy -> do
        plot cv sx (sy - 1) (t (216, 224, 242))
        plot cv sx sy (t (152, 164, 190))
        plot cv (sx + 1) sy (t (94, 102, 130))
      stamp cv t (cx - 12) (cy + 2) skullSmall
    "dead_end" -> forM_ [0 .. 13 :: Int] $ \i -> do
      let rx = x + 21 + hash rid i 71 `mod` 9
          ry = y + 13 + hash rid i 72 `mod` 8
          s  = 2 + hash rid i 73 `mod` 3
      rock cv t rx ry s
    "exit" -> pure ()
    _ -> pure ()
  where (x, y) = origin r

rock :: Canvas s -> Tone -> Int -> Int -> Int -> ST s ()
rock cv t x y s = do
  forM_ [0 .. s] $ \j -> forM_ [0 .. s + 1] $ \i -> do
    let corner = (i == 0 || i == s + 1) && (j == 0 || j == s)
        c | j == 0 || i == 0 = stoneTone 5
          | j == s || i == s + 1 = stoneTone 2
          | otherwise = stoneTone 4
    unless corner (plot cv (x + i) (y + j) (t c))
  forM_ [1 .. s] $ \i -> plot cv (x + i) (y + s + 1) (t (26, 20, 36))
