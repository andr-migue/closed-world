module Screens.Layers
  ( fitsTerminal
  , overBackdrop
  , belowMiddle
  , bottomRow
  , noticeLine
  ) where

import Brick
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Tui.Names (Name)
import Game.Clock (clock)
import Game.State
import Screens.Backdrop (backdrop)
import Style.Theme
import Widgets.Header (header)
import Widgets.KeyBar (keyBar)

minWidth, minHeight :: Int
minWidth  = 120
minHeight = 34

fitsTerminal :: Widget Name -> Widget Name
fitsTerminal w = Widget Greedy Greedy $ do
  ctx <- getContext
  if availWidth ctx < minWidth || availHeight ctx < minHeight
    then render (tooSmall (availWidth ctx) (availHeight ctx))
    else render w

tooSmall :: Int -> Int -> Widget Name
tooSmall w h = center $ vBox
  [ hCenter (withAttr aTitle (str "The terminal is too small"))
  , str " "
  , hCenter (withAttr aLine (str ("Closed World needs at least " ++ show minWidth ++ " × " ++ show minHeight ++ ".")))
  , hCenter (withAttr aDim (str ("Now it is " ++ show w ++ " × " ++ show h ++ ".")))
  ]

overBackdrop :: Game -> Widget Name -> [(String, String)] -> [Widget Name]
overBackdrop game body keys =
  [ bottomRow (keyBar keys)
  , header game Nothing
  , centerLayer body
  , backdrop False (clock game)
  ]

belowMiddle :: Int -> Widget Name -> Widget Name
belowMiddle width w = Widget Greedy Greedy $ do
  ctx <- getContext
  render (translateBy (Location ((availWidth ctx - width) `div` 2, availHeight ctx * 52 `div` 100)) w)

bottomRow :: Widget Name -> Widget Name
bottomRow w = Widget Greedy Greedy $ do
  ctx <- getContext
  render (translateBy (Location (0, availHeight ctx - 1)) (vLimit 1 (hLimit (availWidth ctx) w)))

noticeLine :: Game -> Widget Name
noticeLine game
  | null (notice game) = emptyWidget
  | otherwise          = padLeftRight 1 (withAttr aTrap (str (notice game)))
