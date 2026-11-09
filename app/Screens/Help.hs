module Screens.Help (helpScreen, helpPages) where

import Brick
import Brick.Widgets.Center (hCenter)
import Tui.Names (Name)
import Icons.Characters
import Icons.Places
import Pixels.Icon (iconImage)
import Style.Theme
import Widgets.Panel

helpPages :: [(String, [String])]
helpPages =
  [ ( "The idea"
    , [ "Every level is a Prolog program and a fixed query."
      , "The program is logically correct: the answer exists."
      , "But Prolog's search order keeps the adventurer from reaching it."
      , ""
      , "You cannot rewrite the program. You can only change how it runs:"
      , "reorder clauses or goals, add a cut or remove one,"
      , "with the few tools each level allows."
      ] )
  , ( "The dungeon"
    , [ "The dungeon is the SLD tree of the query."
      , ""
      , "  Room             the goal the adventurer is solving"
      , "  Open door        a clause whose head matches"
      , "  Shut door        a clause whose head does not match"
      , "  Dead end         no clause left: the adventurer goes back"
      , "  Closed door      a door removed by a cut"
      , "  Ghost            not/1 exploring ahead on its own"
      , "  Lit rooms        the current branch; cold, faded ones were left behind"
      , "  Torch            one unit per step: when it runs out, you lose"
      , "  Trap             trap/0 kills the adventurer on the spot"
      ] )
  , ( "Watching a run"
    , [ "  space          release the adventurer, pause or resume"
      , "  ← →            previous / next step"
      , "  Home End       first / last step"
      , "  + -            walk faster / slower"
      , "  W A S D        pan the map, or drag it with the mouse"
      , "  c              centre the map on the adventurer again"
      , "  m              show or hide the minimap"
      , "  f              fluid (30 fps) or eco (15 fps) animation"
      , "  PgUp PgDn [ ]  scroll the program; the wheel scrolls any panel"
      , "  e              edit the program"
      , "  h              show or hide the level's hint"
      , "  ?              this help"
      , "  esc            back to the levels"
      ] )
  , ( "Editing the program"
    , [ "  arrows         select a clause head, a goal or the end of a body"
      , "  space          pick the selection, then drop it somewhere else"
      , "  c              insert a cut at the selection"
      , "  x              remove the selected cut"
      , "  u  R           undo the last edit / all edits"
      , "  enter          release the adventurer with the edited program"
      , "  esc            cancel a pick, or stop editing"
      , ""
      , "A head moves the whole clause; a goal moves within its clause or query."
      ] )
  , ( "Stars"
    , [ "Reaching the treasure earns one star."
      , "One more for not using more torch than the level's par,"
      , "and one more for not using more edits."
      , ""
      , "Matching the best known solution earns all three."
      ] )
  ]

helpScreen :: Int -> Widget Name
helpScreen page = fixedPanel (Panel ("HOW TO PLAY · " ++ title) pager aGold 2) 80 (length body + extra + 6) $ \_ _ ->
  (vBox (str " " : map line body ++ legend), Nothing, Nothing)
  where
    (title, body) = helpPages !! page
    extra = if page == 1 then 7 else 0
    line "" = str " "
    line l  = withAttr aLine (str l)
    pager = "page " ++ show (page + 1) ++ " of " ++ show (length helpPages)
    legend
      | page == 1 = [str " ", hCenter (hBox (map figure figures))]
      | otherwise = []
    figure (s, name) = padLeftRight 1 (vBox [hCenter (raw (iconImage s)), hCenter (withAttr aDim (str name))])
    figures =
      [ (adventurer, "you"), (entrance, "entrance"), (chest, "treasure"), (skull, "trap")
      , (rubble, "dead end"), (ghost, "ghost"), (portal, "exit") ]
