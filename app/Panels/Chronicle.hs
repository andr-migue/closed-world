module Panels.Chronicle (chroniclePanel, chronicleContent) where

import Brick
import qualified Data.Text as T
import Tui.Names (Name (..))
import Prolog.Protocol
import Style.Theme
import Widgets.Panel
import Widgets.Scroll

chroniclePanel :: LevelRun -> Int -> (Int, Int) -> Widget Name
chroniclePanel run k delta = clickable ChronicleView $ reportExtent ChronicleView $
  panel (Panel "CHRONICLE" info aArcane 1) $ \w h -> renderContent (chronicleContent run k w) w h delta
  where info = if k == 0 then "" else "step " ++ show k ++ "/" ++ show (length (runSteps run))

chronicleContent :: LevelRun -> Int -> Int -> Content
chronicleContent run k width = Content allLines 5 (Just (Focus (length allLines - 1) 0 0)) Bottom
  where
    total = length (runSteps run)
    bodyW = max 8 (width - 5)
    intro = [ Line [] [(aMessage, l)] Nothing | l <- wrapText (bodyW - 1) (levelDescription (runLevel run)) ] ++ [Line [] [] Nothing]
    entries = concat [ entry i s | (i, s) <- zip [1 ..] (take k (runSteps run)) ]
    outro
      | k == total && total > 0 = Line [] [] Nothing : [ Line [] [(resultAttr, l)] Nothing | l <- wrapText (bodyW - 1) (runMessage run) ]
      | otherwise = []
    resultAttr = if runResult run == T.pack "success" then aGold else aCrimson
    allLines = intro ++ entries ++ outro
    entry i s =
      let (glyph, attr) = eventIcon (stepEvent s)
          textLines = wrapText (bodyW - 2) (stepText s)
          now = i == k
          fillAttr = if now then Just aEntryCurrent else Nothing
          textAttr = if now then aLine else aKeyLabel
          number = T.pack (replicate (3 - length (show i)) ' ' ++ show i ++ " ")
          first = Line [(if now then aGutterCurrent else aGutter, number)]
                       [(attr, T.pack (glyph ++ " ")), (textAttr, head (textLines ++ [T.empty]))] fillAttr
          rest = [ Line [] [(aDim, T.pack "  "), (textAttr, l)] fillAttr | l <- drop 1 textLines ]
      in first : rest

eventIcon :: T.Text -> (String, AttrName)
eventIcon event = case T.unpack event of
  "enter"        -> ("▾", aGold)
  "mismatch"     -> ("×", aDim)
  "backtrack"    -> ("↶", aSteel)
  "cut"          -> ("!", aCrimson)
  "not_start"    -> ("✦", aArcane)
  "not_fails"    -> ("✦", aArcane)
  "not_succeeds" -> ("✦", aArcane)
  "success"      -> ("◆", aGold)
  "trap"         -> ("☠", aCrimson)
  _              -> ("·", aDim)
