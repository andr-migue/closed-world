module Widgets.Header (header) where

import Brick
import Tui.Names (Name)
import Game.State
import Prolog.Protocol
import Style.Theme

header :: Game -> Maybe LevelRun -> Widget Name
header game mrun = vLimit 1 $ withDefAttr aHeader $ hBox $
  withAttr aBrand (str " ◆ CLOSED WORLD ") : case mrun of
    Just run ->
      [ withAttr aDim (str ("  Level " ++ show (levelId game) ++ "/" ++ show (length (levels game)) ++ "  "))
      , withAttr aPanelTitle (txt (levelName (runLevel run)))
      , withAttr aHeader (fill ' ')
      , hBox (status run)
      ]
    Nothing -> [withAttr aHeader (fill ' ')]
  where
    status run =
      [ withAttr aDim (str "step ")
      , withAttr aValue (str (show (stepIx game)))
      , withAttr aDim (str ("/" ++ show (length (runSteps run)) ++ "   "))
      , withAttr (if playing game then aGold else aDim) (str (if playing game then "▶ playing " else "❚❚ paused "))
      , withAttr aHeader (str " ")
      ]
