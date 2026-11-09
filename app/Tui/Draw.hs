module Tui.Draw (draw) where

import Brick
import Tui.Names (Name)
import Game.Clock (clock, elapsed)
import Game.Editor (Mode (..))
import Game.RunInfo (finished)
import Game.State
import Screens.Backdrop (backdrop)
import Screens.Help (helpScreen)
import Screens.Layers
import Screens.Levels (levelsScreen)
import Screens.Menu (menuScreen)
import Screens.Play (playScreen)
import Screens.Result (resultDialog)
import Widgets.KeyBar (keyBar)

draw :: Game -> [Widget Name]
draw game = map fitsTerminal $ case screen game of
  Menu i ->
    [ bottomRow (keyBar [("↑/↓", "choose"), ("enter", "select"), ("q", "quit")])
    , belowMiddle 40 (menuScreen game i <=> noticeLine game)
    , backdrop True (clock game)
    ]
  Levels i -> overBackdrop game (levelsScreen game i) [("↑/↓", "choose"), ("enter", "play"), ("esc", "menu")]
  Help p _ -> overBackdrop game (helpScreen p) [("←/→", "page"), ("esc", "back")]
  Playing -> case current game of
    Nothing  -> [noticeLine game]
    Just run ->
      [ resultDialog game run | finished game, Replay <- [mode game], elapsed game (endFrame game) > 480 ]
      ++ [ playScreen game run ]
