module Icons.Font (bigText, bigTextWidth, bigTextPixels) where

import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Pixels.Icon (iconRows)
import Style.Colors (RGB)

bigText :: (Int -> RGB) -> RGB -> String -> V.Image
bigText ink shadowInk text = V.vertCat (iconRows (map row [0 .. 7]))
  where
    pixels   = layout text
    width    = maximum (0 : [x + 1 | (x, _) <- M.keys pixels])
    row y    = [cell x y | x <- [0 .. width + 1]]
    cell x y
      | M.member (x, y) pixels         = Just (ink y)
      | M.member (x - 1, y - 1) pixels = Just shadowInk
      | otherwise                      = Nothing

bigTextPixels :: String -> [(Int, Int)]
bigTextPixels = M.keys . layout

bigTextWidth :: String -> Int
bigTextWidth text = sum (map glyphWidth text) + length text + 1

layout :: String -> M.Map (Int, Int) ()
layout = go 0
  where
    go _ [] = M.empty
    go x (c : cs) =
      M.union (M.fromList [((x + gx, gy), ()) | (gy, line) <- zip [0 ..] (glyph c), (gx, '#') <- zip [0 ..] line])
              (go (x + glyphWidth c + 1) cs)

glyphWidth :: Char -> Int
glyphWidth ' ' = 3
glyphWidth c   = length (head (glyph c))

glyph :: Char -> [String]
glyph c = M.findWithDefault (replicate 7 "     ") c glyphs

glyphs :: M.Map Char [String]
glyphs = M.fromList
  [ ('A', [" ### ", "#   #", "#   #", "#####", "#   #", "#   #", "#   #"])
  , ('B', ["#### ", "#   #", "#   #", "#### ", "#   #", "#   #", "#### "])
  , ('C', [" ### ", "#   #", "#    ", "#    ", "#    ", "#   #", " ### "])
  , ('D', ["#### ", "#   #", "#   #", "#   #", "#   #", "#   #", "#### "])
  , ('E', ["#####", "#    ", "#    ", "#### ", "#    ", "#    ", "#####"])
  , ('F', ["#####", "#    ", "#    ", "#### ", "#    ", "#    ", "#    "])
  , ('G', [" ### ", "#   #", "#    ", "# ###", "#   #", "#   #", " ####"])
  , ('H', ["#   #", "#   #", "#   #", "#####", "#   #", "#   #", "#   #"])
  , ('I', [" ### ", "  #  ", "  #  ", "  #  ", "  #  ", "  #  ", " ### "])
  , ('J', ["  ###", "   # ", "   # ", "   # ", "#  # ", "#  # ", " ##  "])
  , ('K', ["#   #", "#  # ", "# #  ", "##   ", "# #  ", "#  # ", "#   #"])
  , ('L', ["#    ", "#    ", "#    ", "#    ", "#    ", "#    ", "#####"])
  , ('M', ["#   #", "## ##", "# # #", "# # #", "#   #", "#   #", "#   #"])
  , ('N', ["#   #", "##  #", "# # #", "#  ##", "#   #", "#   #", "#   #"])
  , ('O', [" ### ", "#   #", "#   #", "#   #", "#   #", "#   #", " ### "])
  , ('P', ["#### ", "#   #", "#   #", "#### ", "#    ", "#    ", "#    "])
  , ('Q', [" ### ", "#   #", "#   #", "#   #", "# # #", "#  # ", " ## #"])
  , ('R', ["#### ", "#   #", "#   #", "#### ", "# #  ", "#  # ", "#   #"])
  , ('S', [" ####", "#    ", "#    ", " ### ", "    #", "    #", "#### "])
  , ('T', ["#####", "  #  ", "  #  ", "  #  ", "  #  ", "  #  ", "  #  "])
  , ('U', ["#   #", "#   #", "#   #", "#   #", "#   #", "#   #", " ### "])
  , ('V', ["#   #", "#   #", "#   #", "#   #", "#   #", " # # ", "  #  "])
  , ('W', ["#   #", "#   #", "#   #", "# # #", "# # #", "# # #", " # # "])
  , ('X', ["#   #", "#   #", " # # ", "  #  ", " # # ", "#   #", "#   #"])
  , ('Y', ["#   #", "#   #", " # # ", "  #  ", "  #  ", "  #  ", "  #  "])
  , ('Z', ["#####", "    #", "   # ", "  #  ", " #   ", "#    ", "#####"])
  , ('!', ["#", "#", "#", "#", "#", " ", "#"])
  ]
