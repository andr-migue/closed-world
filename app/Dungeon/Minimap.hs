module Dungeon.Minimap (minimap) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dungeon.Timeline
import Pixels.Canvas
import Prolog.Protocol
import Style.Colors (RGB)

minimap :: Canvas s -> LevelRun -> [RoomView] -> Viewport -> Int -> ST s ()
minimap cv run shown port time = when (fits && not (null shown) && rows * cols > 1) $ do
  darkenRect cv 0.35 (bx - 2) (by - 2) (mw + 4) (mh + 4)
  forM_ [bx - 2 .. bx + mw + 1] $ \x -> do
    glow cv x (by - 2) frameInk
    glow cv x (by + mh + 1) frameInk
  forM_ [by - 2 .. by + mh + 1] $ \y -> do
    glow cv (bx - 2) y frameInk
    glow cv (bx + mw + 1) y frameInk
  forM_ shown $ \v -> forM_ (rvParent v >>= \p -> M.lookup p byId) $ \parent -> do
    let (x1, y1) = spot parent
        (x2, y2) = spot v
        ink = if rvState v == Abandoned then (60, 58, 84) else (176, 122, 58)
        from = x1 + cw `div` 2
        to = x2 + cw `div` 2
    forM_ [y1 + ch .. y2 - 1] $ \y -> glow cv from y ink
    forM_ [min from to .. max from to] $ \x -> glow cv x (y2 - 1) ink
  forM_ shown $ \v -> do
    let (x, y) = spot v
    forM_ [0 .. cw - 1] $ \i -> forM_ [0 .. ch - 1] $ \j -> glow cv (x + i) (y + j) (roomDot time v)
  where
    frameInk = (70, 64, 104)
    byId = M.fromList [(rvId v, v) | v <- shown]
    cols = maximum (0 : map roomCol (runRooms run)) + 1
    rows = maximum (0 : map roomRow (runRooms run)) + 1
    sizeOf (a, b, c, d) = (cols * a - (a - c), rows * b - (b - d))
    fitsIn s = let (w, h) = sizeOf s in w <= portW port `div` 4 && h <= portH port `div` 3
    (sx, sy, cw, ch) = case filter fitsIn [(4, 4, 3, 2), (3, 3, 2, 2), (2, 2, 1, 1)] of
      (s : _) -> s
      []      -> (2, 2, 1, 1)
    (mw, mh) = sizeOf (sx, sy, cw, ch)
    fits = fitsIn (sx, sy, cw, ch) && portW port > 30
    bx = portX port + portW port - mw - 4
    by = portY port + 4
    spot v = (bx + rvCol v * sx, by + rvRow v * sy)

roomDot :: Int -> RoomView -> RGB
roomDot time v = case (rvState v, T.unpack (rvKind v), rvGhost v) of
  (Current, _, _)
    | even (time `div` 4) -> (255, 248, 214)
    | otherwise -> (255, 196, 92)
  (_, "treasure", _) -> (255, 196, 92)
  (_, "trap", _) -> (214, 58, 82)
  (_, "dead_end", _) -> (96, 92, 116)
  (OnPath, _, True) -> (166, 124, 246)
  (OnPath, _, False) -> (236, 226, 204)
  (Abandoned, _, True) -> (84, 66, 140)
  _ -> (110, 112, 140)
