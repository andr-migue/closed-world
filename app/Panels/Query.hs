module Panels.Query (queryPanel, queryContent) where

import Brick
import qualified Data.Text as T
import Tui.Names (Name (..))
import Game.Editor
import Prolog.Protocol
import Style.Theme
import Widgets.Panel
import Widgets.Scroll
import Widgets.Syntax (segments)

queryPanel :: Mode -> LevelRun -> (Int, Int) -> Widget Name
queryPanel m run delta = clickable QueryView $ reportExtent QueryView $ vLimit 3 $
  panel (Panel "QUERY" "" aGold 1) $ \w h -> renderContent (queryContent m run) w h delta

queryContent :: Mode -> LevelRun -> Content
queryContent m run = case m of
  Editing cursor pick | isQueryRow level (curRow cursor) -> editing cursor pick
  _ -> Content [plainLine ((aGold, T.pack "?- ") : segments (levelQuery level) ++ [(aPunct, T.pack ".")])] 0 Nothing Center
  where
    level = runLevel run
    goals = levelQueryGoals level
    editing cursor pick =
      let row = curRow cursor
          styled (c, t)
            | cursor == Cursor row c = [(aCursor, t)]
            | picked c = [(aPicked, t)]
            | otherwise = segments t
          picked c = case pick of
            Just (PickGoal r c') -> r == row && c' == c
            _ -> False
          pieces = [[(aGold, T.pack "?- ")]]
                   ++ [styled tok ++ [(aPunct, T.pack ", ") | fst tok < length goals] | tok <- zip [1 ..] goals]
                   ++ [[(aCursor, T.pack " ") | curCol cursor == length goals + 1], [(aPunct, T.pack ".")]]
          i = min (length goals + 1) (curCol cursor)
          s = scanl (+) 0 (map segWidth pieces) !! i
      in Content [Line [] (concat pieces) (Just aRowCursor)] 0 (Just (Focus 0 s (s + max 1 (segWidth (pieces !! i))))) Center
