module Panels.Program (programPanel, programContent) where

import Brick
import qualified Data.Text as T
import Tui.Names (Name (..))
import Game.Editor
import Prolog.Protocol
import Style.Theme
import Widgets.Panel
import Widgets.Scroll
import Widgets.Syntax (segments)

programPanel :: Mode -> LevelRun -> Int -> (Int, Int) -> Widget Name
programPanel m run k delta = clickable ProgramView $ reportExtent ProgramView $
  panel (Panel "PROGRAM" info aGold 1) $ \w h -> renderContent (programContent m run k) w h delta
  where
    info = case m of
      Editing _ (Just _) -> "drop it somewhere"
      Editing _ Nothing  -> "editing"
      Replay -> maybe (show (length (levelProgram (runLevel run))) ++ " clauses") (\c -> "clause " ++ show c) (activeClause run k)

programContent :: Mode -> LevelRun -> Int -> Content
programContent m run k = case m of
  Replay -> Content (zipWith replayLine [1 ..] (levelProgram level)) gutterWidth (fmap (\i -> Focus (i - 1) 0 0) active) Center
  Editing cursor pick ->
    let rows = [editRow cursor pick r parts | (r, parts) <- zip [1 ..] (levelClauses level)]
        focus = case [Focus (r - 1) s e | (r, (_, Just (s, e))) <- zip [1 ..] rows] of
          (f : _) -> Just f
          []      -> Nothing
        gutter r = [(if curRow cursor == r then aGutterCurrent else aGutter, T.pack (number r ++ " "))]
        fill' r = if curRow cursor == r then Just aRowCursor else Nothing
    in Content [Line (gutter r) body (fill' r) | (r, (body, _)) <- zip [1 ..] rows] gutterWidth focus Center
  where
    level = runLevel run
    digits = length (show (max 1 (length (levelProgram level))))
    gutterWidth = digits + 4
    number i = replicate (digits - length (show i)) ' ' ++ show (i :: Int)
    active = activeClause run k
    replayLine i text
      | Just i == active = Line [(aGutterCurrent, T.pack ("▸ " ++ number i ++ " "))] (segments text) (Just aLineCurrent)
      | otherwise        = Line [(aGutter, T.pack ("  " ++ number i ++ " "))] (segments text) Nothing

activeClause :: LevelRun -> Int -> Maybe Int
activeClause run k
  | k > 0 && k <= length (runSteps run) = stepClause (runSteps run !! (k - 1))
  | otherwise = Nothing

editRow :: Cursor -> Maybe Pick -> Int -> ClauseParts -> ([Seg], Maybe (Int, Int))
editRow cursor pick row parts = (concat pieces, focus)
  where
    goals = clauseGoals parts
    tokens = (0, clauseHead parts, True) : [(c, g, False) | (c, g) <- zip [1 ..] goals]
    separator (c, _, isHead)
      | isHead = [(aPunct, T.pack " :- ") | not (null goals)]
      | c < length goals = [(aPunct, T.pack ", ")]
      | otherwise = []
    endSlot = [(aCursor, T.pack " ") | cursor == Cursor row (length goals + 1)]
    styled (c, t, _)
      | cursor == Cursor row c = [(aCursor, t)]
      | picked c = [(aPicked, t)]
      | otherwise = segments t
    picked c = case pick of
      Just (PickClause r)  -> r == row
      Just (PickGoal r c') -> r == row && c' == c
      Nothing              -> False
    pieces = [styled tok ++ separator tok | tok <- tokens] ++ [endSlot, [(aPunct, T.pack ".")]]
    starts = scanl (+) 0 (map segWidth pieces)
    focus
      | curRow cursor /= row = Nothing
      | otherwise =
          let i = min (length tokens) (curCol cursor)
              s = starts !! i
          in Just (s, s + max 1 (segWidth (concat (take 1 (drop i pieces)))))
