module Pixels.Terminal
  ( Label (..)
  , toImage
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Unboxed as U
import qualified Graphics.Vty as V
import Graphics.Vty.Image.Internal (Image (HorizText))
import Pixels.Canvas (Picture, Viewport (..))
import Pixels.Color
import Pixels.Light (Lighting, shade)
import Style.Colors (RGB, color)

data Label = Label
  { labelX    :: !Int
  , labelY    :: !Int
  , labelText :: T.Text
  , labelInk  :: RGB
  , labelBold :: Bool
  }

toImage :: Viewport -> Lighting -> [Label] -> Picture -> V.Image
toImage port lighting labels pic = V.vertCat [row cy | cy <- [0 .. rows - 1]]
  where
    w = portW port
    rows = portH port `div` 2
    pixels = shade port lighting pic
    letters = IM.fromList
      [ (cy * w + cx, (ch, labelInk l, labelBold l))
      | l <- labels
      , let cy = (labelY l - portY port) `div` 2
      , (k, ch) <- zip [0 ..] (T.unpack (labelText l))
      , let cx = labelX l - portX port + k
      , cx >= 0, cx < w, cy >= 0, cy < rows
      ]
    at x y = U.unsafeIndex pixels (y * w + x)
    row cy = V.horizCat (cells 0)
      where
        top x = at x (2 * cy)
        bottom x = at x (2 * cy + 1)
        letterAt x = if IM.null letters then Nothing else IM.lookup (cy * w + x) letters
        cells x
          | x >= w = []
          | Just (ch, ink, bold) <- letterAt x =
              let bg = mixRGB 0.5 (unpack (top x)) (unpack (bottom x))
                  a = V.defAttr `V.withForeColor` color ink `V.withBackColor` color bg
              in V.char (if bold then a `V.withStyle` V.bold else a) ch : cells (x + 1)
          | otherwise =
              let t = top x
                  b = bottom x
                  n = sameRun t b (x + 1) - x
                  (ch, a)
                    | t == b    = (' ', V.defAttr `V.withBackColor` color (unpack t))
                    | otherwise = ('▀', V.defAttr `V.withForeColor` color (unpack t) `V.withBackColor` color (unpack b))
              in HorizText a (TL.replicate (fromIntegral n) (TL.singleton ch)) n n : cells (x + n)
        sameRun t b x
          | x < w && top x == t && bottom x == b && letterAt x == Nothing = sameRun t b (x + 1)
          | otherwise = x
