module Screens.Result (resultDialog) where

import Brick
import Brick.Widgets.Center (centerLayer, hCenter)
import qualified Data.Text as T
import Tui.Names (Name)
import Game.Clock (elapsed)
import Game.RunInfo
import Game.State
import Icons.Font (bigText)
import Icons.Places (chest, rubble, skull)
import Icons.Stars (starsImage)
import Icons.Torch (torchOut)
import Panels.Torch (parText)
import Pixels.Icon (iconImage)
import Prolog.Protocol
import Style.Colors
import Style.Theme
import Widgets.KeyBar (chip)
import Widgets.Panel

resultDialog :: Game -> LevelRun -> Widget Name
resultDialog game run = centerLayer $ fixedPanel (Panel title "" accent 2) 76 (if won then 20 else 16) $ \_ _ ->
  ( vBox $
      [ str " "
      , hCenter (hBox [raw (iconImage icon), str "   ", raw (bigText ink outline word)])
      , str " "
      , hCenter (withAttr aMessage (txt (subtitle (runMessage run))))
      ]
      ++ [ padTop (Pad 1) (hCenter (raw (starsImage revealed))) | won ]
      ++ [ str " "
         , hCenter (hBox [ withAttr aLabel (str "Torch used "), withAttr aValue (str (show (runTorchUsed run)))
                         , withAttr aDim (str (" / " ++ show (levelTorch level) ++ "     "))
                         , withAttr aLabel (str "Edits "), withAttr aValue (str (show (levelEdits level))) ])
         , hCenter (withAttr aDim (str ("Par: " ++ parText (levelPar level))))
         , str " "
         , hCenter (hBox (actions game))
         ]
  , Nothing, Nothing )
  where
    level = runLevel run
    won = succeeded game
    revealed = min (runStars run) (elapsed game (endFrame game) `div` 360)
    title = if won then "TREASURE" else "THE ADVENTURER FAILED"
    accent = if won then aGold else aCrimson
    (word, icon, ink) = case T.unpack (runResult run) of
      "success"   -> ("TREASURE", chest, gradient goldLight gold goldDeep)
      "trap"      -> ("TRAP", skull, gradient (255, 120, 130) crimson crimsonDeep)
      "torch_out" -> ("TORCH OUT", torchOut, gradient flameLight ember crimsonDeep)
      _           -> ("NO WAY OUT", rubble, gradient steel wallLight wallShadow)

subtitle :: T.Text -> T.Text
subtitle m = maybe m T.strip (T.stripPrefix (T.pack "TREASURE!") m)

gradient :: RGB -> RGB -> RGB -> Int -> RGB
gradient top middle bottom y
  | y <= 1    = top
  | y <= 4    = middle
  | otherwise = bottom

actions :: Game -> [Widget Name]
actions game
  | succeeded game =
      [ chip "n" "next level" | hasNextLevel game ]
      ++ [ chip "e" "improve", chip "space" "watch again", chip "esc" "levels" ]
  | otherwise =
      [ chip "e" "edit", chip "h" "hint", chip "space" "watch again", chip "esc" "levels" ]
