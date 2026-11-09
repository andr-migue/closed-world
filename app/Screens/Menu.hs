module Screens.Menu (menuScreen) where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Data.Map.Strict as M
import Tui.Names (Name)
import Game.State
import Style.Theme
import Widgets.Panel

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
