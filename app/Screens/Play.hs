module Screens.Play (playScreen) where

import Brick
import qualified Data.Text as T
import Tui.Names (Name (..))
import Dungeon.Scene
import Game.Clock (clock, progress)
import Game.Editor (Mode (..))
import Game.RunInfo (torchRatio)
import Game.State
import Panels.Chronicle
import Panels.Hint
import Panels.Program
import Panels.Query
import Panels.Torch
import Prolog.Protocol
import Style.Theme
import Widgets.Header
import Widgets.KeyBar
import Widgets.Panel

playScreen :: Game -> LevelRun -> Widget Name
playScreen game run = vBox
  [ cached CacheHeader (header game (Just run))
  , hBox [mapPanel game run, sidebar]
  , cached CacheFooter (footer game)
  ]
  where
    k = stepIx game
    sidebar = hLimit 50 $ vBox $
      [ torchPanel game run k
      , cached CacheProgram (programPanel (mode game) run k (scrollOf ProgramView game))
      , cached CacheQuery (queryPanel (mode game) run (scrollOf QueryView game))
      ]
      ++ [ hintPanel run | hint game ]
      ++ [ vLimitPercent 38 (cached CacheChronicle (chroniclePanel run k (scrollOf ChronicleView game))) ]

mapPanel :: Game -> LevelRun -> Widget Name
mapPanel game run = clickable DungeonView $ panel (Panel "DUNGEON" info aGold 0) $ \w h ->
  (hLimit w (vLimit h (dungeon scene)), Nothing, Nothing)
  where
    k = stepIx game
    scene = Scene
      { scRun      = run
      , scStep     = k
      , scProgress = if k == 0 then 1 else progress game
      , scTime     = clock game
      , scPan      = pan game
      , scTorch    = torchRatio run k
      , scMinimap  = minimap game
      , scCamera   = camera game
      }
    info
      | pan game /= (0, 0) = "panned · c to follow"
      | otherwise = T.unpack (levelConcept (runLevel run))

footer :: Game -> Widget Name
footer game
  | not (null (notice game)) =
      vLimit 1 (hBox [withAttr aNotice (str ("  ⚠ " ++ notice game ++ "  ")), withAttr aHeader (fill ' ')])
  | otherwise = keyBar $ case mode game of
      Editing _ _ ->
        [ ("arrows", "select"), ("space", "pick/drop"), ("c", "cut"), ("x", "uncut")
        , ("u", "undo"), ("R", "reset"), ("pgup/dn", "scroll"), ("enter", "run"), ("esc", "done") ]
      Replay
        | stepIx game == 0 && not (playing game) ->
            [ ("space", "release the adventurer"), ("e", "edit"), ("h", "hint"), ("m", "map")
            , ("?", "help"), ("esc", "levels") ]
        | otherwise ->
            [ ("space", if playing game then "pause" else "play"), ("←/→", "step"), ("+/-", "speed")
            , ("wasd", "pan"), ("c", "centre"), ("e", "edit"), ("h", "hint"), ("pgup/dn", "scroll"), ("?", "help"), ("esc", "levels") ]
