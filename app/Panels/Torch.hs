module Panels.Torch (torchPanel, parText) where

import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Tui.Names (Name)
import Game.Clock (clock)
import Game.RunInfo (torchLeft)
import Game.State
import Icons.Torch
import Pixels.Icon (iconImage)
import Prolog.Protocol
import Style.Colors
import Style.Theme
import Widgets.Panel

torchPanel :: Game -> Run -> Int -> Widget Name
torchPanel game run k = Widget Greedy Fixed $ do
  ctx <- getContext
  let w = availWidth ctx
      chipRows = packChips (w - 10) (map toolChip (levelTools level))
  render $ fixedPanel (Panel "TORCH" (if left <= 0 then "burnt out" else "") aFlame 1) w (8 + length chipRows) $ \iw _ ->
    ( vBox $
        [ hBox [ raw (iconImage icon), str "  ", vBox [ raw (torchBar (iw - 9) ratio (clock game)), counts iw ] ]
        , str " "
        ]
        ++ [ hBox (withAttr aLabel (str (if i == 0 then "Tools " else "      ")) : map snd row) | (i, row) <- zip [0 :: Int ..] chipRows ]
        ++ [ hBox [ withAttr aLabel (str "Edits "), withAttr aValue (str (show (levelEdits level)))
                  , withAttr aDim (str "   par "), withAttr aValue (str (parText (levelPar level))) ] ]
    , Nothing, Nothing )
  where
    level = runLevel run
    torch = levelTorch level
    left  = torchLeft run k
    ratio = fromIntegral (max 0 left) / fromIntegral (max 1 torch) :: Double
    icon
      | left <= 0 = torchOut
      | even (clock game `div` 3) = flame1
      | otherwise = flame2
    counts iw = hLimit (iw - 9) $ hBox
      [ withAttr aValue (str (show (max 0 left))), withAttr aDim (str (" / " ++ show torch))
      , padLeft Max (speedMeter (speed game))
      ]

speedMeter :: Int -> Widget Name
speedMeter s = hBox
  [ withAttr aDim (str "speed ")
  , withAttr aGold (str (replicate (s + 1) '▸'))
  , withAttr aPanelTrack (str (replicate (3 - s) '▸'))
  ]

parText :: Maybe Par -> String
parText Nothing    = "none"
parText (Just par) = show (parTorch par) ++ " torch · " ++ show (parEdits par) ++ " edits"

toolChip :: Tool -> (Int, Widget Name)
toolChip t = (length label + 1, hBox [withAttr attr (str label), str " "])
  where
    label = " " ++ glyph ++ " " ++ name ++ " ×" ++ show (toolUses t) ++ " "
    (glyph, kind, name) = case T.unpack (toolName t) of
      "move_clauses" -> ("⇅", aChip, "clause")
      "move_goals"   -> ("⇄", aChip, "goal")
      "add_cut"      -> ("!", aChipCut, "cut")
      "remove_cut"   -> ("✂", aChipCut, "uncut")
      other          -> ("·", aChip, other)
    attr = if toolUses t <= 0 then aChipOff else kind

packChips :: Int -> [(Int, Widget Name)] -> [[(Int, Widget Name)]]
packChips width = go 0 []
  where
    go _ acc [] = [reverse acc | not (null acc)]
    go used acc (c@(n, _) : rest)
      | null acc || used + n <= width = go (used + n) (c : acc) rest
      | otherwise = reverse acc : go 0 [] (c : rest)

torchBar :: Int -> Double -> Int -> V.Image
torchBar w ratio time = V.vertCat [row 0, row 1]
  where
    width = max 6 w
    filled = round (ratio * fromIntegral (width - 2)) :: Int
    (light, middle, dark)
      | ratio > 0.5  = ((255, 222, 130), (255, 160, 60), (212, 100, 40))
      | ratio > 0.25 = ((255, 170, 90), (222, 92, 52), (150, 52, 36))
      | otherwise    = ((255, 120, 130), (214, 58, 82), (132, 30, 56))
    edge = (70, 60, 96)
    shimmer = (time `div` 2) `mod` (width + 10)
    pixel x y
      | x == 0 || x == width - 1 = if y == 0 || y == 3 then Nothing else Just edge
      | y == 0 || y == 3 = Just edge
      | x - 1 < filled = Just (if abs (x - shimmer) <= 1 && y == 1 then (255, 250, 230) else [light, middle, dark] !! (y - 1))
      | otherwise = Just (if y == 1 then (42, 38, 58) else (34, 30, 48))
    row r = V.horizCat [cell (pixel x (2 * r)) (pixel x (2 * r + 1)) | x <- [0 .. width - 1]]
    cell (Just a) (Just b) = V.char (V.defAttr `V.withForeColor` color a `V.withBackColor` color b) '▀'
    cell (Just a) Nothing  = V.char (V.defAttr `V.withForeColor` color a `V.withBackColor` color panelBg) '▀'
    cell Nothing (Just b)  = V.char (V.defAttr `V.withForeColor` color b `V.withBackColor` color panelBg) '▄'
    cell Nothing Nothing   = V.char (V.defAttr `V.withBackColor` color panelBg) ' '
