module UI.Play (playScreen, resultDialog, playKeys) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Center (centerLayer, hCenter)
import qualified Data.Text as T
import Editor
import Protocol
import State
import UI.Dungeon
import UI.Palette
import UI.Panels
import UI.Pixel (spriteImage)
import UI.PixelFont (banner)
import UI.Sprites (chest, rubble, skull, starsImage, torchOut)
import UI.Theme

playScreen :: Game -> Run -> Widget Name
playScreen game run = vBox
  [ hBox [ padAll 1 (dungeon (follow game) run k)
         , vBorder
         , hLimit 52 (vBox [programPanel (mode game) run k, statusPanel (frame game) (hint game) run k])
         ]
  , hBorder
  , if null (notice game)
      then playKeys game
      else padLeftRight 1 (withAttr aTrap (str (notice game)))
  ]
  where k = stepIx game

resultDialog :: Game -> Run -> Widget Name
resultDialog game run = centerLayer $ hLimit 84 $ borderWithLabel (withAttr aTitle (str label)) $ padAll 1 $ vBox $
  [ hCenter (hBox [raw (spriteImage icon), str "   ", raw (banner ink outline word)])
  , str " "
  , hCenter (withAttr aMessage (txt (subtitle (runMessage run))))
  ]
  ++ [ padTop (Pad 1) (hCenter (raw (starsImage (runStars run)))) | won ]
  ++ [ str " "
     , hCenter (hBox [ withAttr aLabel (str "Torch used "), withAttr aValue (str (show (runTorchUsed run)))
                     , withAttr aDim (str (" / " ++ show (levelTorch level) ++ "     "))
                     , withAttr aLabel (str "Edits "), withAttr aValue (str (show (levelEdits level))) ])
     , hCenter (withAttr aDim (str ("Par: " ++ parText (levelPar level))))
     , str " "
     , hCenter (hBox (actions game))
     ]
  where
    level = runLevel run
    won   = succeeded game
    label = if won then " Treasure " else " The adventurer failed "
    (word, icon, ink) = case T.unpack (runResult run) of
      "success"   -> ("TREASURE", chest, inkOf goldLight gold goldDeep)
      "trap"      -> ("TRAP", skull, inkOf (255, 120, 130) crimson crimsonDeep)
      "torch_out" -> ("TORCH OUT", torchOut, inkOf flameLight ember crimsonDeep)
      _           -> ("NO WAY OUT", rubble, inkOf steel wallLight wallShadow)

subtitle :: T.Text -> T.Text
subtitle m = maybe m T.strip (T.stripPrefix (T.pack "TREASURE!") m)

inkOf :: RGB -> RGB -> RGB -> Int -> RGB
inkOf top middle bottom y
  | y <= 1    = top
  | y <= 4    = middle
  | otherwise = bottom

actions :: Game -> [Widget Name]
actions game
  | succeeded game =
      [ key "n" "next level" | hasNextLevel game ]
      ++ [ key "e" "improve", key "space" "watch again", key "esc" "levels" ]
  | otherwise =
      [ key "e" "edit", key "h" "hint", key "space" "watch again", key "esc" "levels" ]

playKeys :: Game -> Widget Name
playKeys game = padLeftRight 1 $ hBox $ case mode game of
  Editing _ _ ->
    [ key "arrows" "select", key "space" "pick/drop", key "c" "cut", key "x" "uncut"
    , key "u" "undo", key "R" "reset", key "enter" "run", key "esc" "stop editing" ]
  Replay
    | stepIx game == 0 && not (playing game) ->
        [ key "space" "release the adventurer", key "e" "edit", key "h" "hint"
        , key "?" "help", key "esc" "levels" ]
    | otherwise ->
        [ key "space" (if playing game then "pause" else "play"), key "←/→" "step"
        , key "wasd" "pan", key "e" "edit", key "h" "hint", key "?" "help", key "esc" "levels" ]

key :: String -> String -> Widget Name
key k label = withAttr aKey (str (" " ++ k ++ " ")) <+> str (" " ++ label ++ "   ")
