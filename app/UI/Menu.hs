module UI.Menu (menuScreen, levelsScreen) where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Data.Map.Strict as M
import Protocol
import State
import UI.Frame
import UI.Pixel
import UI.Sprites
import UI.Theme

menuScreen :: Game -> Int -> Widget Name
menuScreen game selected =
  fixedPanel (Panel "ENTER THE DUNGEON" "" aGold 2) 40 (length menuItems + 6) $ \_ _ ->
      ( vBox ([str " "] ++ [hCenter (item i (menuLabel m)) | (i, m) <- zip [0 ..] menuItems] ++ [str " ", hCenter (progressLine game)])
      , Nothing, Nothing )
  where
    item i label
      | i == selected = withAttr aSelected (str ("  ▸ " ++ label ++ "    "))
      | otherwise     = withAttr aLine (str ("    " ++ label ++ "    "))

progressLine :: Game -> Widget Name
progressLine game
  | null (levels game) = emptyWidget
  | otherwise = hBox
      [ withAttr aGold (str "★ "), withAttr aValue (str (show stars)), withAttr aDim (str (" / " ++ show (3 * n) ++ "   "))
      , withAttr aEmerald (str "◆ "), withAttr aValue (str (show solved)), withAttr aDim (str (" / " ++ show n ++ " solved"))
      ]
  where
    n      = length (levels game)
    solved = M.size (best game)
    stars  = sum (M.elems (best game))

levelsScreen :: Game -> Int -> Widget Name
levelsScreen game selected = hBox
  [ fixedPanel (Panel "LEVELS" (show (M.size (best game)) ++ "/" ++ show (length (levels game)) ++ " solved") aGold 2) 50 (max 20 (length (levels game) + 4)) $ \_ _ ->
      ( vBox (str " " : if null (levels game) then [withAttr aTrap (str "No levels available.")] else zipWith row [0 ..] (levels game))
      , Nothing, Nothing )
  , str "  "
  , card
  ]
  where
    row i summary = (if i == selected then withDefAttr aLineCurrent else id) $ hBox
      [ withAttr aGold (str (if i == selected then "▸ " else "  "))
      , withAttr (if i == selected then aGold else aDim) (str (pad2 (summaryId summary) ++ "  "))
      , withAttr (if i == selected then aValue else aLine) (padRight Max (txt (summaryName summary)))
      , starsLine (M.lookup (summaryId summary) (best game))
      ]
    pad2 n = if n < 10 then ' ' : show n else show n
    card = case drop selected (levels game) of
      (summary : _) -> levelCard game summary
      []            -> emptyWidget

starsLine :: Maybe Int -> Widget Name
starsLine Nothing  = withAttr aDim (str "· · · ")
starsLine (Just n) = withAttr aGold (str (replicate n '★')) <+> withAttr aDim (str (replicate (3 - n) '☆' ++ " "))

levelCard :: Game -> LevelSummary -> Widget Name
levelCard game summary = fixedPanel (Panel ("LEVEL " ++ show (summaryId summary)) "" aArcane 2) 50 (max 20 (length (levels game) + 4)) $ \_ _ ->
  ( vBox
      [ str " "
      , hCenter (raw (spriteImage (levelIcon (summaryId summary))))
      , str " "
      , hCenter (withAttr aValue (txt (summaryName summary)))
      , hCenter (withAttr aGold (txt (summaryConcept summary)))
      , str " "
      , withAttr aMessage (txtWrap (summaryDescription summary))
      , padTop Max (hCenter (raw (starsImage (maybe 0 id (M.lookup (summaryId summary) (best game))))))
      ]
  , Nothing, Nothing )
