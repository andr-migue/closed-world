module Panels.Hint (hintPanel) where

import Brick
import qualified Data.Text as T
import Tui.Names (Name)
import Prolog.Protocol
import Style.Theme
import Widgets.Panel
import Widgets.Scroll (wrapText)

hintPanel :: Run -> Widget Name
hintPanel run = Widget Greedy Fixed $ do
  ctx <- getContext
  let w = availWidth ctx
      textLines = wrapText (w - 5) (hintText (levelHint (runLevel run)))
  render $ fixedPanel (Panel "HINT" "h to hide" aEmerald 1) w (length textLines + 2) $ \_ _ ->
    (vBox [withAttr aHint (txt l) | l <- textLines], Nothing, Nothing)

hintText :: T.Text -> T.Text
hintText h
  | T.null h  = T.pack "No hint for this level."
  | otherwise = h
