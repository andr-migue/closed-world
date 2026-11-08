module UI.Dungeon.Scene
  ( Scene (..)
  , dungeon
  , sceneFrame
  , sceneImage
  ) where

import Brick
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Graphics.Vty as V
import Protocol
import UI.Art
import UI.Canvas
import UI.Dungeon.Layout
import UI.Dungeon.Paint
import UI.Dungeon.View
import UI.Palette (RGB)
import UI.Theme (Name (..))

data Scene = Scene
  { scRun      :: Run
  , scStep     :: Int
  , scProgress :: Double
  , scTime     :: Int
  , scPan      :: (Int, Int)
  , scTorch    :: Double
  , scMinimap  :: Bool
  }

dungeon :: Scene -> Widget Name
dungeon sc = Widget Greedy Greedy $ do
  ctx <- getContext
  let w = max 1 (availWidth ctx)
      h = max 1 (availHeight ctx)
  render (raw (sceneImage sc w h))

sceneImage :: Scene -> Int -> Int -> V.Image
sceneImage sc w h = let (fr, px) = sceneFrame sc w h in canvasImage fr px

data Cast = Cast
  { castHero  :: Maybe Actor
  , castGhost :: Maybe Actor
  }

sceneFrame :: Scene -> Int -> Int -> (Frame, (U.Vector Int, U.Vector Bool))
sceneFrame sc w h = (fr, pixels)
  where
    run   = scRun sc
    k     = scStep sc
    t     = scTime sc
    ly    = layout run
    mv    = mapView run k
    views = M.fromList [(rvId v, v) | v <- mvRooms mv]
    rooms = [r | r <- runRooms run, M.member (roomId r) views]
    room i = M.lookup i (lyRooms ly)
    viewOf r = M.lookup (roomId r) views
    cast  = castAt ly run k (scProgress sc)
    focus = case castGhost cast of
      Just g | actorAlpha g > 0.3 -> Just g
      _ -> castHero cast
    pw = w
    ph = h * 2
    (fx, fy) = maybe (fromIntegral (lyWidth ly) / 2, fromIntegral (lyHeight ly) / 2)
                     (\a -> (actorX a, actorY a - 6)) focus
    camX0 = round fx - pw `div` 2 + fst (scPan sc)
    camY0 = round fy - ph `div` 2 + snd (scPan sc)
    camX = fit camX0 pw (lyWidth ly)
    camY = 2 * (fit camY0 ph (lyHeight ly) `div` 2)
    fit c view world
      | world <= view = (world - view) `div` 2
      | otherwise     = max (-margin) (min (world - view + margin) c)
    looks r = case viewOf r of
      Just v  -> lookOf v
      Nothing -> Look memoryTone False False False
    label r = fit' (roomLabel r)
    fit' s
      | T.length s <= roomW - 8 = s
      | otherwise = T.take (roomW - 9) s <> T.pack "…"
    corridors = sortOn (lookLit . corLook)
      [ Corridor (slotX s) (fst (origin c) + roomW `div` 2) (snd (origin p) + roomH) (barY p) (snd (origin c)) (looks c)
      | c <- rooms
      , Just pid <- [roomParent c]
      , Just p <- [room pid]
      , M.member pid views
      , Just s <- [listToMaybe [s | s <- M.findWithDefault [] pid (lySlots ly), doorTarget (slotDoor s) == Just (roomId c), doorStep (slotDoor s) <= k]]
      ]
    visibleSlots r = [s | s <- M.findWithDefault [] (roomId r) (lySlots ly), doorStep (slotDoor s) <= k]
    torchR = 16 + 30 * scTorch sc + flick 0 * 1.5
    flick salt = fromIntegral (hash (t `div` 2) salt 3 `mod` 100) / 100 - 0.5
    heroLight = case castHero cast of
      Just a | scTorch sc > 0 ->
        [Light (actorX a + 3) (actorY a - 8) torchR (1.0, 0.74, 0.46) (1.3 * actorAlpha a)]
      _ -> []
    ghostLight = case castGhost cast of
      Just g -> [Light (actorX g) (actorY g - 6) 16 (0.62, 0.5, 1.0) (0.9 * actorAlpha g)]
      Nothing -> []
    roomLights r = case viewOf r of
      Nothing -> []
      Just v ->
        [ Light (fromIntegral (sconceX s) + 0.5) (fromIntegral (sconceY s) + 1) (17 + flick (sconceX s) * 2) (1.0, 0.66, 0.36) 0.75
        | lookLit (lookOf v), s <- sconces r ]
        ++ kindLights r v
    kindLights r v = case T.unpack (rvKind v) of
      "treasure" -> let (cx, cy) = chestCenter r in [Light (fromIntegral cx) (fromIntegral cy) 26 (1.0, 0.84, 0.4) 1.1]
      "trap"     -> let (cx, cy) = trapCenter r in [Light (fromIntegral cx) (fromIntegral cy) 22 (1.0, 0.25, 0.3) (0.8 + 0.3 * sin (fromIntegral t / 3))]
      "exit"     -> let (cx, cy) = portalCenter r in [Light (fromIntegral cx) (fromIntegral cy) 22 (0.6, 0.45, 1.0) 1.0]
      _          -> []
    flashes = eventLights ly run k (scProgress sc)
    ambient = if scTorch sc <= 0 then 0.18 else 0.62
    labels =
      concat
        [ [ Label lx (py + 0) (label r) (roomInk v) (rvState v == Current) ]
          ++ [ Label (slotX s - T.length num `div` 2) (snd (origin r) + roomH) num (doorInk (lookOf v) (doorStatus (slotDoor s))) False
             | s <- visibleSlots r, Just cl <- [doorClause (slotDoor s)], let num = T.pack (show cl)
             , slotWide s || cl < 10 ]
        | r <- rooms, Just v <- [viewOf r]
        , let (px0, py, pwid) = plaqueLabel r (T.length (label r))
        , let lx = px0 + (pwid - T.length (label r)) `div` 2
        ]
    fr = Frame pw ph camX camY ambient (heroLight ++ ghostLight ++ concatMap roomLights rooms ++ flashes) labels True
    pixels = paint fr (12, 11, 19) $ \cv -> do
      voidRock cv camX camY pw ph
      forM_ rooms $ \r -> roomShell cv (looks r) r
      forM_ corridors (corridorWalls cv)
      forM_ corridors (corridorFloor cv)
      forM_ rooms $ \r -> forM_ (visibleSlots r) (doorway cv (looks r) r)
      forM_ rooms $ \r -> case viewOf r of
        Just v  -> roomDetails cv t (lookOf v) r (rvKind v) (T.length (label r))
        Nothing -> pure ()
      forM_ corridors (corridorTorch cv t)
      forM_ rooms $ \r -> maybe (pure ()) (kindEffects cv t r) (viewOf r)
      eventEffects cv ly run k (scProgress sc) t
      drawActors cv t cast (scTorch sc)
      motes cv t cast
      when (scMinimap sc) (minimap cv run mv camX camY pw ph t)

lookOf :: RoomView -> Look
lookOf v = Look tone lit (rvGhost v) (rvState v == Current)
  where
    lit = rvState v /= Abandoned
    tone = case (lit, rvGhost v) of
      (True, False)  -> plain
      (True, True)   -> ghostTone
      (False, False) -> memoryTone
      (False, True)  -> memoryTone . ghostTone

roomInk :: RoomView -> RGB
roomInk v = case (rvState v, rvGhost v) of
  (Current, False)   -> (255, 222, 130)
  (Current, True)    -> (196, 176, 255)
  (OnPath, False)    -> (236, 226, 204)
  (OnPath, True)     -> (176, 160, 236)
  (Abandoned, _)     -> (110, 112, 140)

doorInk :: Look -> T.Text -> RGB
doorInk lk status
  | not (lookLit lk) = (84, 86, 110)
  | otherwise = case T.unpack status of
      "open"   -> (255, 196, 92)
      "ghost"  -> (166, 124, 246)
      "closed" -> (226, 72, 96)
      _        -> (122, 128, 156)

heroRoom :: Layout -> Run -> Int -> Maybe Int
heroRoom ly run j = listToMaybe [roomId r | r <- ancestry ly =<< maybe [] pure (known run j), not (roomGhost r)]

ghostRoom :: Layout -> Run -> Int -> Maybe Int
ghostRoom ly run j = case known run j >>= \i -> M.lookup i (lyRooms ly) of
  Just r | roomGhost r -> Just (roomId r)
  _ -> Nothing

known :: Run -> Int -> Maybe Int
known run j = listToMaybe (mapMaybe (currentRoom run) [j, j - 1 .. 0])

eventAt :: Run -> Int -> Maybe Step
eventAt run k
  | k >= 1 && k <= length (runSteps run) = Just (runSteps run !! (k - 1))
  | otherwise = Nothing

isEvent :: String -> Maybe Step -> Bool
isEvent name = maybe False ((== T.pack name) . stepEvent)

castAt :: Layout -> Run -> Int -> Double -> Cast
castAt ly run k p
  | k == 0 = Cast (stand <$> (room 0)) Nothing
  | otherwise = Cast hero ghost
  where
    room i = M.lookup i (lyRooms ly)
    stand r = let (x, y) = standPoint r in Actor x y FaceDown 0 1
    e = eventAt run k
    q = min 1 (p / 0.85)
    stepDoor = listToMaybe [d | d <- runDoors run, doorStep d == k]
    hf = heroRoom ly run (k - 1)
    ht = heroRoom ly run k
    gp = ghostRoom ly run (k - 1)
    gc = ghostRoom ly run k
    heroBase = case (hf, ht) of
      (Just a, Just b)
        | isEvent "mismatch" e, Nothing <- gc, Just d <- stepDoor, Just r <- room b -> Just (walk (bump ly r d) q)
        | otherwise -> Just (walk (route ly a b) q)
      (_, Just b) -> stand <$> room b
      _ -> Nothing
    hero = case heroBase of
      Just a | isEvent "trap" e -> Just a { actorAlpha = if p > 0.6 then 0 else if odd (floor (p * 12) :: Int) then 0.3 else 1 }
      other -> other
    ghost = case (gp, gc) of
      (_, Just c)
        | isEvent "mismatch" e, Just d <- stepDoor, Just r <- room c -> Just (walk (bump ly r d) q)
      (Just a, Just c) -> Just (walk (route ly a c) q)
      (Nothing, Just c) -> case ht of
        Just h -> Just ((walk (route ly h c) q) { actorAlpha = min 1 (0.3 + q) })
        Nothing -> stand <$> room c
      (Just a, Nothing)
        | isEvent "not_fails" e -> (\r -> (stand r) { actorAlpha = 1 - q }) <$> room a
        | Just h <- ht -> Just ((walk (route ly a h) q) { actorAlpha = 1 - q })
      _ -> Nothing

eventLights :: Layout -> Run -> Int -> Double -> [Light]
eventLights ly run k p = case eventAt run k of
  Nothing -> []
  Just s -> case T.unpack (stepEvent s) of
    "cut" -> [ Light (fromIntegral x) (fromIntegral y) 9 (1.0, 0.2, 0.3) (1.6 * (1 - p)) | (x, y) <- doorsAt "closed" ]
    "mismatch" -> [ Light (fromIntegral x) (fromIntegral y) 7 (1.0, 0.3, 0.3) (1.4 * pulse) | (x, y) <- doorsAt "shut" ]
    "success" -> [ Light (fromIntegral cx) (fromIntegral cy) (8 + 40 * p) (1.0, 0.86, 0.45) (1.5 * (1 - p * 0.5)) | Just r <- [here], let (cx, cy) = chestCenter r ]
    "trap" -> [ Light (fromIntegral cx) (fromIntegral cy) (10 + 30 * p) (1.0, 0.15, 0.2) 1.6 | Just r <- [here], let (cx, cy) = trapCenter r ]
    "not_start" -> [ Light (fromIntegral cx) (fromIntegral cy + 4) (6 + 20 * p) (0.6, 0.45, 1.0) (1.4 * (1 - p)) | Just r <- [here], let (cx, cy) = origin r, let _ = cy ]
    _ -> []
  where
    here = currentRoom run k >>= \i -> M.lookup i (lyRooms ly)
    pulse = max 0 (1 - abs (p - 0.45) * 4)
    doorsAt status =
      [ (slotX s, snd (origin r) + 26)
      | (rid, ss) <- M.toList (lySlots ly), Just r <- [M.lookup rid (lyRooms ly)]
      , s <- ss, doorStep (slotDoor s) == k, doorStatus (slotDoor s) == T.pack status ]

eventEffects :: Canvas s -> Layout -> Run -> Int -> Double -> Int -> ST s ()
eventEffects cv ly run k p t = case eventAt run k of
  Nothing -> pure ()
  Just s -> case T.unpack (stepEvent s) of
    "cut" -> forM_ (closedDoors ly k) $ \(x, y) ->
      forM_ [0 .. 5 :: Int] $ \i -> do
        let a = fromIntegral i * 1.05 + p * 3
            d = 2 + p * 6
        when (p < 0.9) $ glow cv (x + round (cos a * d)) (y + round (sin a * d * 0.6) - 2) (255, 110, 120)
    "success" -> forM_ (currentRoom run k >>= \i -> M.lookup i (lyRooms ly)) $ \r -> do
      let (cx, cy) = chestCenter r
      forM_ [0 .. 9 :: Int] $ \i -> do
        let a = fromIntegral i * 0.63 + fromIntegral t * 0.05
            d = 4 + p * 12
        sparkle cv (cx + round (cos a * d)) (cy - 2 + round (sin a * d * 0.6) - round (p * 4))
    "dead_end" -> pure ()
    _ -> pure ()

closedDoors :: Layout -> Int -> [(Int, Int)]
closedDoors ly k =
  [ (slotX s, snd (origin r) + 25)
  | (rid, ss) <- M.toList (lySlots ly), Just r <- [M.lookup rid (lyRooms ly)]
  , s <- ss, doorStep (slotDoor s) == k, doorStatus (slotDoor s) == T.pack "closed" ]

sparkle :: Canvas s -> Int -> Int -> ST s ()
sparkle cv x y = do
  glow cv x y (255, 250, 220)
  glow cv (x - 1) y (255, 210, 110)
  glow cv (x + 1) y (255, 210, 110)
  glow cv x (y - 1) (255, 210, 110)
  glow cv x (y + 1) (255, 210, 110)

corridorTorch :: Canvas s -> Int -> Corridor -> ST s ()
corridorTorch cv t c = do
  let lo = min (corDoorX c) (corChildX c)
      hi = max (corDoorX c) (corChildX c)
      mid = (lo + hi) `div` 2
      y = corBar c - 4
  when (hi - lo >= 24) $ do
    plot cv mid (y + 1) (lookTone (corLook c) (94, 102, 130))
    if lookLit (corLook c)
      then do
        glow cv mid y (if even (t `div` 2) then (255, 196, 86) else (255, 140, 52))
        glow cv mid (y - 1) (255, 140, 52)
      else plot cv mid y (lookTone (corLook c) (60, 52, 50))

kindEffects :: Canvas s -> Int -> Room -> RoomView -> ST s ()
kindEffects cv t r v = case T.unpack (rvKind v) of
  "exit" -> do
    let (cx, cy) = portalCenter r
    forM_ [0 .. 59 :: Int] $ \i -> do
      let a = fromIntegral i / 60 * 2 * pi
          ex = cx + round (cos a * 7)
          ey = cy + round (sin a * 5)
          hot = (i + t) `mod` 12 < 3
      glow cv ex ey (if hot then (240, 230, 255) else (150, 112, 236))
    forM_ [0 .. 23 :: Int] $ \i -> do
      let a = fromIntegral i / 24 * 2 * pi + fromIntegral t * 0.15
          d = 2 + fromIntegral (i `mod` 4)
      glow cv (cx + round (cos a * d)) (cy + round (sin a * d * 0.7)) (if odd i then (130, 92, 222) else (156, 236, 250))
  "treasure" -> do
    let (cx, cy) = chestCenter r
    forM_ [0 .. 2 :: Int] $ \i -> do
      let phase = (t + i * 7) `mod` 21
      when (phase < 4) $ sparkle cv (cx - 4 + hash (roomId r) i ((t + i * 7) `div` 21) `mod` 9) (cy - 6 + hash i (roomId r) ((t + i * 7) `div` 21) `mod` 4)
  _ -> when (rvState v /= Abandoned) $ forM_ (sconces r) $ \s ->
    forM_ [0 .. 1 :: Int] $ \i -> do
      let life = (t + i * 5 + sconceX s) `mod` 10
          ex = sconceX s + (hash (sconceX s) i ((t + i * 5) `div` 10) `mod` 3) - 1
      when (life < 6) $ blend cv (1 - fromIntegral life / 7) ex (sconceY s - 1 - life) (255, 160, 70)

drawActors :: Canvas s -> Int -> Cast -> Double -> ST s ()
drawActors cv t cast torch = do
  forM_ (castHero cast) $ \a -> when (actorAlpha a > 0) $ do
    let x = round (actorX a) - 4
        y = round (actorY a) - 11
        frames = case actorFacing a of
          FaceDown  -> heroDown
          FaceUp    -> heroUp
          _         -> heroSide
        sprite0 = frames !! actorStride a
        sprite = if actorFacing a == FaceLeft then mirrored sprite0 else sprite0
        bob = if actorStride a == 0 && even (t `div` 6) then 0 else 0
    shadow cv (round (actorX a)) (round (actorY a))
    when (actorFacing a /= FaceUp) $ torchProp cv t a torch
    if actorAlpha a >= 0.99
      then stamp cv id x (y + bob) sprite
      else stampBlend cv (actorAlpha a) id x (y + bob) sprite
    when (actorFacing a == FaceUp) $ torchProp cv t a torch
  forM_ (castGhost cast) $ \g -> when (actorAlpha g > 0.05) $ do
    let hover = round (sin (fromIntegral t / 2.5) * 1.2 :: Double)
        x = round (actorX g) - 4
        y = round (actorY g) - 13 + hover
    stampBlend cv (0.75 * actorAlpha g) id x y (spirit !! ((t `div` 3) `mod` 2))
    forM_ [0 .. 3 :: Int] $ \i -> do
      let life = (t + i * 3) `mod` 12
      blend cv (0.5 * actorAlpha g * (1 - fromIntegral life / 12)) (x + 1 + (i * 2) `mod` 7) (y + 9 + life `div` 3) (190, 168, 255)

torchProp :: Canvas s -> Int -> Actor -> Double -> ST s ()
torchProp cv t a torch = do
  let (dx, dy) = case actorFacing a of
        FaceDown  -> (3, -4)
        FaceUp    -> (-4, -5)
        FaceLeft  -> (-4, -4)
        FaceRight -> (3, -4)
      x = round (actorX a) + dx
      y = round (actorY a) + dy
  plot cv x y (122, 78, 50)
  plot cv x (y + 1) (78, 46, 34)
  when (torch > 0) $ do
    let tall = 1 + round (torch * 2) :: Int
        sway = if even (t `div` 2) then 0 else 1
    glow cv x (y - 1) (255, 246, 200)
    glow cv (x - 1 + sway) (y - 1) (255, 196, 86)
    forM_ [2 .. tall] $ \j -> glow cv (x + (if odd j then sway else 0)) (y - j) (if j == tall then (250, 132, 46) else (255, 196, 86))
  when (torch <= 0 && even (t `div` 3)) $ blend cv 0.5 x (y - 2 - (t `div` 3) `mod` 3) (124, 116, 104)

shadow :: Canvas s -> Int -> Int -> ST s ()
shadow cv x y = do
  forM_ [x - 3 .. x + 2] $ \px -> darken cv 0.55 px y
  forM_ [x - 2 .. x + 1] $ \px -> darken cv 0.7 px (y + 1)

motes :: Canvas s -> Int -> Cast -> ST s ()
motes cv t cast = forM_ (castHero cast) $ \a -> forM_ [0 .. 7 :: Int] $ \i -> do
  let period = 40 + i * 7
      phase = (t + i * 13) `mod` period
      seed = (t + i * 13) `div` period
      ox = fromIntegral (hash i seed 81 `mod` 30) - 15
      oy = fromIntegral (hash i seed 82 `mod` 20) - 16
      drift = fromIntegral phase * 0.15
      px = round (actorX a + ox + sin (fromIntegral phase / 6) * 1.5)
      py = round (actorY a + oy - drift)
      fade = sin (fromIntegral phase / fromIntegral period * pi)
  blend cv (0.55 * fade) px py (255, 226, 170)

minimap :: Canvas s -> Run -> MapView -> Int -> Int -> Int -> Int -> Int -> ST s ()
minimap cv run mv camX camY pw ph t = when (fits && not (null shown) && rows * cols > 1) $ do
  darkenRect cv 0.35 (bx - 2) (by - 2) (mw + 4) (mh + 4)
  forM_ [bx - 2 .. bx + mw + 1] $ \x -> do
    glow cv x (by - 2) (70, 64, 104)
    glow cv x (by + mh + 1) (70, 64, 104)
  forM_ [by - 2 .. by + mh + 1] $ \y -> do
    glow cv (bx - 2) y (70, 64, 104)
    glow cv (bx + mw + 1) y (70, 64, 104)
  forM_ shown $ \v -> forM_ (rvParent v >>= \p -> M.lookup p byId) $ \pv -> do
    let (x1, y1) = spot pv
        (x2, y2) = spot v
        ink = if rvState v == Abandoned then (60, 58, 84) else (176, 122, 58)
    let mx = x1 + cw `div` 2
        nx = x2 + cw `div` 2
    forM_ [y1 + ch .. y2 - 1] $ \y -> glow cv mx y ink
    forM_ [min mx nx .. max mx nx] $ \x -> glow cv x (y2 - 1) ink
  forM_ shown $ \v -> do
    let (x, y) = spot v
        ink = case (rvState v, T.unpack (rvKind v), rvGhost v) of
          (Current, _, _)
            | even (t `div` 4) -> (255, 248, 214)
            | otherwise -> (255, 196, 92)
          (_, "treasure", _) -> (255, 196, 92)
          (_, "trap", _) -> (214, 58, 82)
          (_, "dead_end", _) -> (96, 92, 116)
          (OnPath, _, True) -> (166, 124, 246)
          (OnPath, _, False) -> (236, 226, 204)
          (Abandoned, _, True) -> (84, 66, 140)
          _ -> (110, 112, 140)
    forM_ [0 .. cw - 1] $ \i -> forM_ [0 .. ch - 1] $ \j -> glow cv (x + i) (y + j) ink
  where
    shown = mvRooms mv
    byId = M.fromList [(rvId v, v) | v <- shown]
    cols = maximum (0 : map roomCol (runRooms run)) + 1
    rows = maximum (0 : map roomRow (runRooms run)) + 1
    scales = [(4, 4, 3, 2), (3, 3, 2, 2), (2, 2, 1, 1)]
    sizeOf (a, b, c, d) = (cols * a - (a - c), rows * b - (b - d))
    ok s = let (mw', mh') = sizeOf s in mw' <= pw `div` 4 && mh' <= ph `div` 3
    (sx, sy, cw, ch) = case filter ok scales of
      (s : _) -> s
      []      -> (2, 2, 1, 1)
    (mw, mh) = sizeOf (sx, sy, cw, ch)
    fits = ok (sx, sy, cw, ch) && pw > 30
    bx = camX + pw - mw - 4
    by = camY + 4
    spot v = (bx + rvCol v * sx, by + rvRow v * sy)
