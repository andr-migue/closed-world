module Widgets.KeyBar (keyBar, chip) where

import Brick
import Tui.Names (Name)
import Style.Theme

keyBar :: [(String, String)] -> Widget Name
keyBar pairs = vLimit 1 $ hBox (withAttr aHeader (str " ") : map (uncurry chip) pairs ++ [withAttr aHeader (fill ' ')])

chip :: String -> String -> Widget Name
chip key label = withAttr aKey (str (" " ++ key ++ " ")) <+> withAttr aChipLabel (str (" " ++ label ++ "  "))
