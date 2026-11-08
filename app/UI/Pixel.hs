module UI.Pixel (Sprite, spriteRows, clearRows, spriteImage, spriteWidth, scaled, blank) where

import qualified Graphics.Vty as V
import UI.Palette

type Sprite = [[Maybe RGB]]

spriteWidth :: Sprite -> Int
spriteWidth []      = 0
spriteWidth (r : _) = length r

spriteRows :: (RGB -> RGB) -> RGB -> Sprite -> [V.Image]
spriteRows shade background = pairs cell
  where
    cell top bottom = V.char (attr (pick top) (pick bottom)) '▀'
    pick = maybe background shade
    attr f b = V.defAttr `V.withForeColor` color f `V.withBackColor` color b

clearRows :: (RGB -> RGB) -> Sprite -> [V.Image]
clearRows shade = pairs cell
  where
    cell (Just t) (Just b) = V.char (V.defAttr `V.withForeColor` color (shade t) `V.withBackColor` color (shade b)) '▀'
    cell (Just t) Nothing  = V.char (V.defAttr `V.withForeColor` color (shade t) `V.withBackColor` color panelBg) '▀'
    cell Nothing (Just b)  = V.char (V.defAttr `V.withForeColor` color (shade b) `V.withBackColor` color panelBg) '▄'
    cell Nothing Nothing   = V.char (V.defAttr `V.withBackColor` color panelBg) ' '

scaled :: Sprite -> Sprite
scaled = concatMap (\r -> [double r, double r])
  where double = concatMap (\p -> [p, p])

spriteImage :: Sprite -> V.Image
spriteImage = V.vertCat . clearRows id

pairs :: (Maybe RGB -> Maybe RGB -> V.Image) -> Sprite -> [V.Image]
pairs cell = go
  where
    go (top : bottom : rest) = V.horizCat (zipWith cell top bottom) : go rest
    go [top]                 = [V.horizCat (zipWith cell top (map (const Nothing) top))]
    go []                    = []

blank :: RGB -> Int -> V.Image
blank background n = V.charFill (V.defAttr `V.withBackColor` color background) ' ' n 1
