module UI.Menu (menuScreen, levelsScreen) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import qualified Data.Map.Strict as M
import Protocol
import State
import UI.Palette
import UI.Pixel
import UI.PixelFont
import UI.Sprites
import UI.Theme

menuScreen :: Game -> Int -> Widget Name
menuScreen game selected = center $ vBox
  [ hCenter (hBox [torch, padLeftRight 3 (raw title), torch])
  , str " "
  , hCenter (withAttr aMessage (str "A puzzle game where every level is a Prolog program"))
  , str " "
  , str " "
  , hCenter (vBox [item i (menuLabel m) | (i, m) <- zip [0 ..] menuItems])
  , str " "
  , str " "
  , hCenter (progressLine game)
  ]
  where
    title = banner titleInk outline "CLOSED WORLD"
    torch = raw (spriteImage (if even (frame game) then torchTall else flicker torchTall))
    item i label
      | i == selected = withAttr aSelected (str ("   " ++ label ++ "   "))
      | otherwise     = withAttr aLine (str ("   " ++ label ++ "   "))

titleInk :: Int -> RGB
titleInk y = case y of
  0 -> goldLight
  1 -> goldLight
  2 -> gold
  3 -> gold
  4 -> gold
  5 -> goldDeep
  _ -> goldDeep

flicker :: Sprite -> Sprite
flicker s = case s of
  (a : b : rest) -> map (map warm) [a, b] ++ rest
  other          -> other
  where warm = fmap (\c -> if c == flameLight then flame else if c == flame then flameLight else c)

progressLine :: Game -> Widget Name
progressLine game
  | null (levels game) = emptyWidget
  | otherwise = hBox
      [ withAttr aGold (str "★ "), withAttr aValue (str (show stars)), withAttr aDim (str (" / " ++ show (3 * n) ++ "   "))
      , withAttr aEmerald (str "◆ "), withAttr aValue (str (show solved)), withAttr aDim (str (" / " ++ show n ++ " levels solved"))
      ]
  where
    n      = length (levels game)
    solved = M.size (best game)
    stars  = sum (M.elems (best game))

levelsScreen :: Game -> Int -> Widget Name
levelsScreen game selected = center $ hBox
  [ borderWithLabel (withAttr aTitle (str " Levels ")) $ padAll 1 $ hLimit 44 $ vBox $
      if null (levels game)
        then [withAttr aTrap (str "No levels available.")]
        else zipWith row [0 ..] (levels game)
  , str "  "
  , card
  ]
  where
    row i summary = hBox
      [ withAttr aGold (str (if i == selected then "▸ " else "  "))
      , withAttr (if i == selected then aGold else aDim) (str (show (summaryId summary) ++ "  "))
      , withAttr (if i == selected then aGold else aLine) (padRight Max (txt (summaryName summary)))
      , starsLine (M.lookup (summaryId summary) (best game))
      ]
    card = case drop selected (levels game) of
      (summary : _) -> levelCard game summary
      []            -> emptyWidget

starsLine :: Maybe Int -> Widget Name
starsLine Nothing  = withAttr aDim (str "· · ·")
starsLine (Just n) = withAttr aGold (str (replicate n '★')) <+> withAttr aDim (str (replicate (3 - n) '☆'))

levelCard :: Game -> LevelSummary -> Widget Name
levelCard game summary = borderWithLabel (withAttr aTitle (str (" Level " ++ show (summaryId summary) ++ " "))) $
  padAll 1 $ hLimit 46 $ vBox
    [ hCenter (raw (spriteImage (levelIcon (summaryId summary))))
    , str " "
    , hCenter (withAttr aValue (txt (summaryName summary)))
    , hCenter (withAttr aGold (txt (summaryConcept summary)))
    , str " "
    , withAttr aMessage (txtWrap (summaryDescription summary))
    , str " "
    , hCenter (raw (starsImage (maybe 0 id (M.lookup (summaryId summary) (best game)))))
    ]
