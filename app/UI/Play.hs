module UI.Play (playScreen, resultDialog, playKeys) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Center (centerLayer, hCenter)
import Editor
import Protocol
import State
import UI.Dungeon
import UI.Panels
import UI.Theme

playScreen :: Game -> Run -> Widget Name
playScreen game run = vBox
  [ hBox [ padAll 1 (dungeon run k)
         , vBorder
         , hLimit 52 (vBox [programPanel (mode game) run k, statusPanel (hint game) run k])
         ]
  , hBorder
  , if null (notice game)
      then playKeys game
      else padLeftRight 1 (withAttr aTrap (str (notice game)))
  ]
  where k = stepIx game

resultDialog :: Game -> Run -> Widget Name
resultDialog game run = centerLayer $ hLimit 78 $ borderWithLabel (str label) $ padAll 1 $ vBox $
  [ hCenter (withAttr headline (txt (runMessage run))) ]
  ++ [ hCenter (withAttr aTreasure (str (starsText (runStars run)))) | won ]
  ++ [ str " "
     , hCenter (str ("Torch used " ++ show (runTorchUsed run) ++ " / " ++ show (levelTorch level)
                     ++ "   Edits " ++ show (levelEdits level)))
     , hCenter (withAttr aDim (str ("Par: " ++ parText (levelPar level))))
     , str " "
     , hCenter (hBox (actions game))
     ]
  where
    level    = runLevel run
    won      = succeeded game
    label    = if won then " Treasure " else " The adventurer failed "
    headline = if won then aTreasure else aTrap

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
        , key "e" "edit", key "h" "hint", key "?" "help", key "esc" "levels" ]

key :: String -> String -> Widget Name
key k label = withAttr aKey (str (" " ++ k ++ " ")) <+> str (" " ++ label ++ "   ")
