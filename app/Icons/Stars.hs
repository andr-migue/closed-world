module Icons.Stars (starsImage) where

import qualified Graphics.Vty as V
import Icons.Inks (icon)
import Pixels.Icon (Icon, iconImage)
import Style.Colors (color, panelBg)

starsImage :: Int -> V.Image
starsImage earned = V.horizCat (concat [[iconImage (pick i), gap] | i <- [1 .. 3]])
  where
    pick i = if i <= earned then star else starEmpty
    gap = V.charFill (V.defAttr `V.withBackColor` color panelBg) ' ' 1 3

star :: Icon
star = icon
  [ "...y..."
  , "..yGy.."
  , "yyyGyyy"
  , ".gGGGg."
  , ".gG.Gg."
  , "gG...Gg"
  ]

starEmpty :: Icon
starEmpty = icon
  [ "...S..."
  , "..S.S.."
  , "SSS.SSS"
  , ".S...S."
  , ".S.S.S."
  , "SS...SS"
  ]
