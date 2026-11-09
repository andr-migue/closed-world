module Dungeon.Doorway (doorway) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Text as T
import Dungeon.Layout
import Dungeon.Textures (cobbleColor)
import Dungeon.Tones
import Pixels.Canvas
import Pixels.Color (scaleRGB)
import Prolog.Protocol
import Sprites.Doors
import Sprites.Sprite
import Style.Colors (stone)

doorway :: Canvas s -> Look -> Room -> Slot -> ST s ()
doorway cv lk r s = case T.unpack (doorStatus (slotDoor s)) of
  "open"   -> opening False
  "ghost"  -> opening True
  "shut"   -> stamp cv t left (y + 24) (if slotWide s then doorShut else doorShutNarrow)
  "closed" -> stampGlow cv sealTone left (y + 24) (if slotWide s then doorSealed else doorSealedNarrow)
  _        -> pure ()
  where
    (_, y) = origin r
    x = slotX s
    t = lookTone lk
    left = if slotWide s then x - 2 else x - 1
    sealTone = if lookLit lk then id else scaleRGB 0.55 . t
    opening ghostly = do
      forM_ [y + 24 .. y + roomH - 1] $ \py -> forM_ [x - 2 .. x + 1] $ \px ->
        plot cv px py (t (cobbleColor px py))
      forM_ [y + 24 .. y + roomH - 1] $ \py -> do
        plot cv (x - 3) py (t (if ghostly then (150, 112, 236) else stone 4))
        plot cv (x + 2) py (t (if ghostly then (96, 70, 170) else stone 2))
      when ghostly $ forM_ [x - 2 .. x + 1] $ \px ->
        glow cv px (y + roomH - 2) (t (170, 140, 255))
