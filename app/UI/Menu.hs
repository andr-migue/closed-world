module UI.Menu (menuScreen, levelsScreen) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Protocol
import State
import UI.Panels (starsText)
import UI.Theme

menuScreen :: Game -> Int -> Widget Name
menuScreen game selected = center $ vBox
  [ hCenter (withAttr aTitle (str "C L O S E D   W O R L D"))
  , hCenter (withAttr aMessage (str "A puzzle game where every level is a Prolog program"))
  , str " "
  , str " "
  , hCenter (vBox [item i (menuLabel m) | (i, m) <- zip [0 ..] menuItems])
  , str " "
  , hCenter (withAttr aDim (str (progressText game)))
  ]
  where
    item i label
      | i == selected = withAttr aSelected (str ("  ▶ " ++ label ++ "  "))
      | otherwise     = withAttr aLine (str ("    " ++ label ++ "  "))

progressText :: Game -> String
progressText game
  | null (levels game) = ""
  | otherwise = show solved ++ " / " ++ show (length (levels game)) ++ " levels solved   "
                ++ show stars ++ " / " ++ show (3 * length (levels game)) ++ " stars"
  where
    solved = M.size (best game)
    stars  = sum (M.elems (best game))

levelsScreen :: Game -> Int -> Widget Name
levelsScreen game selected = center $ hLimit 92 $ borderWithLabel (str " Choose a level ") $
  padAll 1 $ vBox $
    if null (levels game)
      then [withAttr aTrap (str "No levels available.")]
      else zipWith row [0 ..] (levels game)
  where
    row i summary = (if i == selected then withAttr aSelected else withAttr aLine) $ hBox
      [ str (if i == selected then " ▶ " else "   ")
      , hLimit 4 (padRight Max (str (show (summaryId summary))))
      , hLimit 26 (padRight Max (txt (summaryName summary)))
      , hLimit 48 (padRight Max (txt (T.take 47 (summaryConcept summary))))
      , str (maybe "   " starsText (M.lookup (summaryId summary) (best game)))
      , str " "
      ]
