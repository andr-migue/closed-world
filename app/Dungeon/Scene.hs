module Dungeon.Scene
  ( Scene (..)
  , dungeon
  , sceneImage
  , sceneFocus
  ) where

import Brick
import Control.Monad (forM_, when)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Tui.Names (Name)
import Dungeon.Actors
import Dungeon.Camera
import Dungeon.Cast
import Dungeon.Corridor
import Dungeon.Decor
import Dungeon.Doorway
import Dungeon.Effects
import Dungeon.Features
import Dungeon.Layout
import Dungeon.Lights
import Dungeon.Minimap
import Dungeon.Room
import Dungeon.Textures (voidRock)
import Dungeon.Timeline
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Light (Lighting (..))
import Pixels.Terminal
import Prolog.Protocol

data Scene = Scene
  { scRun      :: LevelRun
  , scStep     :: Int
  , scProgress :: Double
  , scTime     :: Int
  , scPan      :: (Int, Int)
  , scTorch    :: Double
  , scMinimap  :: Bool
  , scCamera   :: Maybe (Double, Double)
  }

dungeon :: Scene -> Widget Name
dungeon sc = Widget Greedy Greedy $ do
  ctx <- getContext
  render (raw (sceneImage sc (max 1 (availWidth ctx)) (max 1 (availHeight ctx))))

sceneFocus :: LevelRun -> Int -> Double -> (Double, Double)
sceneFocus run k p = let ly = layout run in focusOf ly (castAt ly run k p)

sceneImage :: Scene -> Int -> Int -> V.Image
sceneImage sc w h = toImage port lighting labels picture
  where
    run = scRun sc
    k = scStep sc
    time = scTime sc
    ly = layout run
    shown = roomsAt run k
    views = M.fromList [(rvId v, v) | v <- shown]
    rooms = [(r, v) | r <- runRooms run, Just v <- [M.lookup (roomId r) views]]
    cast = castAt ly run k (scProgress sc)
    (camX, camY) = cameraCorner ly (w, 2 * h) (fromMaybe (focusOf ly cast) (scCamera sc)) (scPan sc)
    port = Viewport w (2 * h) camX camY
    lighting = Lighting
      { ambient  = if scTorch sc <= 0 then 0.18 else 0.62
      , lights   = sceneLights ly run k (scProgress sc) time (scTorch sc) cast rooms
      , vignette = True
      }
    lookAt rid = maybe forgotten lookOf (M.lookup rid views)
    plaqueText r = shorten (roomLabel r)
    corridors = sortOn (lookLit . corLook) (mapCorridors ly k views rooms lookAt)
    openedDoors r = [s | s <- M.findWithDefault [] (roomId r) (lySlots ly), doorStep (slotDoor s) <= k]
    labels = concat [roomLabels r v | (r, v) <- rooms]
    roomLabels r v =
      let text = plaqueText r
          (x0, y0, width) = plaqueArea r (T.length text)
      in Label (x0 + (width - T.length text) `div` 2) y0 text (plaqueInk v) (rvState v == Current)
         : [ Label (slotX s - T.length number `div` 2) (snd (origin r) + roomH) number (doorInk (lookOf v) (doorStatus (slotDoor s))) False
           | s <- openedDoors r, Just clause <- [doorClause (slotDoor s)]
           , let number = T.pack (show clause), slotWide s || clause < 10 ]
    picture = paint port (12, 11, 19) $ \cv -> do
      voidRock cv camX camY w (2 * h)
      forM_ rooms $ \(r, v) -> roomShell cv (lookOf v) r
      forM_ corridors (corridorWalls cv)
      forM_ corridors (corridorFloor cv)
      forM_ rooms $ \(r, v) -> forM_ (openedDoors r) (doorway cv (lookOf v) r)
      forM_ rooms $ \(r, v) -> do
        let lk = lookOf v
        roomPlaque cv lk r (T.length (plaqueText r))
        roomTorches cv time lk r
        decorate cv lk r (rvKind v) (T.length (plaqueText r))
        feature cv lk r (rvKind v)
      forM_ corridors (corridorTorch cv time)
      forM_ rooms $ \(r, v) -> roomEffects cv time r v
      stepEffects cv ly run k (scProgress sc) time
      drawCast cv time cast (scTorch sc)
      when (scMinimap sc) (minimap cv run shown port time)

mapCorridors :: Layout -> Int -> M.Map Int RoomView -> [(Room, RoomView)] -> (Int -> Look) -> [Corridor]
mapCorridors ly k views rooms lookAt =
  [ Corridor (slotX s) (fst (origin child) + roomW `div` 2) (snd (origin parent) + roomH) (barY parent) (snd (origin child)) (lookAt (roomId child))
  | (child, _) <- rooms
  , Just pid <- [roomParent child]
  , M.member pid views
  , Just parent <- [M.lookup pid (lyRooms ly)]
  , Just s <- [listToMaybe [s | s <- M.findWithDefault [] pid (lySlots ly), doorTarget (slotDoor s) == Just (roomId child), doorStep (slotDoor s) <= k]]
  ]

shorten :: T.Text -> T.Text
shorten s
  | T.length s <= roomW - 8 = s
  | otherwise = T.take (roomW - 9) s <> T.pack "…"
