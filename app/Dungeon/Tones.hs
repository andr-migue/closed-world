module Dungeon.Tones
  ( Look (..)
  , lookOf
  , forgotten
  , plaqueInk
  , doorInk
  ) where

import qualified Data.Text as T
import Dungeon.Timeline
import Pixels.Color
import Style.Colors (RGB)

data Look = Look
  { lookTone :: Tone
  , lookLit  :: Bool
  }

lookOf :: RoomView -> Look
lookOf v = Look tone lit
  where
    lit = rvState v /= Abandoned
    tone = case (lit, rvGhost v) of
      (True, False)  -> id
      (True, True)   -> ghostTone
      (False, False) -> memoryTone
      (False, True)  -> memoryTone . ghostTone

forgotten :: Look
forgotten = Look memoryTone False

memoryTone :: Tone
memoryTone c = scaleRGB 0.5 (mixRGB 0.6 c (l * 8 `div` 10, l * 85 `div` 100, min 255 (l * 118 `div` 100)))
  where l = luma c

ghostTone :: Tone
ghostTone c = mixRGB 0.5 c (l * 72 `div` 100, l * 58 `div` 100, min 255 (l * 140 `div` 100 + 18))
  where l = luma c

plaqueInk :: RoomView -> RGB
plaqueInk v = case (rvState v, rvGhost v) of
  (Current, False) -> (255, 222, 130)
  (Current, True)  -> (196, 176, 255)
  (OnPath, False)  -> (236, 226, 204)
  (OnPath, True)   -> (176, 160, 236)
  (Abandoned, _)   -> (110, 112, 140)

doorInk :: Look -> T.Text -> RGB
doorInk lk status
  | not (lookLit lk) = (84, 86, 110)
  | otherwise = case T.unpack status of
      "open"   -> (255, 196, 92)
      "ghost"  -> (166, 124, 246)
      "closed" -> (226, 72, 96)
      _        -> (122, 128, 156)
