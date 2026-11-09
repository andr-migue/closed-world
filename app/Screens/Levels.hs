module Screens.Levels (levelsScreen) where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Tui.Names (Name)
import Game.State
import Icons.LevelIcons (levelIcon)
import Icons.Stars (starsImage)
import Pixels.Icon (iconImage)
import Prolog.Protocol
import Style.Theme
import Widgets.Panel

levelsScreen :: Game -> Int -> Widget Name
levelsScreen game selected = hBox [levelList game selected height, str "  ", card]
  where
    height = max 20 (length (levels game) + 4)
    card = case drop selected (levels game) of
      (summary : _) -> levelCard game summary height
      []            -> emptyWidget

levelList :: Game -> Int -> Int -> Widget Name
levelList game selected height =
  fixedPanel (Panel "LEVELS" (show (M.size (best game)) ++ "/" ++ show (length (levels game)) ++ " solved") aGold 2) 50 height $ \_ _ ->
    ( vBox (str " " : if null (levels game) then [withAttr aTrap (str "No levels available.")] else zipWith row [0 ..] (levels game))
    , Nothing, Nothing )
  where
    row i summary = (if i == selected then withDefAttr aLineCurrent else id) $ hBox
      [ withAttr aGold (str (if i == selected then "▸ " else "  "))
      , withAttr (if i == selected then aGold else aDim) (str (twoDigits (summaryId summary) ++ "  "))
      , withAttr (if i == selected then aValue else aLine) (padRight Max (txt (summaryName summary)))
      , starsText (M.lookup (summaryId summary) (best game))
      ]
    twoDigits n = if n < 10 then ' ' : show n else show n

starsText :: Maybe Int -> Widget Name
starsText Nothing  = withAttr aDim (str "· · · ")
starsText (Just n) = withAttr aGold (str (replicate n '★')) <+> withAttr aDim (str (replicate (3 - n) '☆' ++ " "))

levelCard :: Game -> LevelSummary -> Int -> Widget Name
levelCard game summary height = fixedPanel (Panel ("LEVEL " ++ show (summaryId summary)) "" aArcane 2) 50 height $ \_ _ ->
  ( vBox
      [ str " "
      , hCenter (raw (iconImage (levelIcon (summaryId summary))))
      , str " "
      , hCenter (withAttr aValue (txt (summaryName summary)))
      , hCenter (withAttr aGold (txt (summaryConcept summary)))
      , str " "
      , withAttr aMessage (txtWrap (summaryDescription summary))
      , padTop Max (hCenter (raw (starsImage (fromMaybe 0 (M.lookup (summaryId summary) (best game))))))
      ]
  , Nothing, Nothing )
