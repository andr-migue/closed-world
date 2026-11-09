module Pixels.Icon
  ( Icon
  , iconRows
  , iconImage
  ) where

import qualified Graphics.Vty as V
import Style.Colors

type Icon = [[Maybe RGB]]

iconImage :: Icon -> V.Image
iconImage = V.vertCat . iconRows

iconRows :: Icon -> [V.Image]
iconRows = pairs
  where
    pairs (top : bottom : rest) = V.horizCat (zipWith cell top bottom) : pairs rest
    pairs [top]                 = [V.horizCat (zipWith cell top (map (const Nothing) top))]
    pairs []                    = []
    cell (Just t) (Just b) = V.char (V.defAttr `V.withForeColor` color t `V.withBackColor` color b) '▀'
    cell (Just t) Nothing  = V.char (V.defAttr `V.withForeColor` color t `V.withBackColor` color panelBg) '▀'
    cell Nothing (Just b)  = V.char (V.defAttr `V.withForeColor` color b `V.withBackColor` color panelBg) '▄'
    cell Nothing Nothing   = V.char (V.defAttr `V.withBackColor` color panelBg) ' '
