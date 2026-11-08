module UI.Help (helpScreen, helpPages) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import UI.Theme

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
      , "  Torch            one unit per step: when it runs out, you lose"
      , "  Trap             trap/0 kills the adventurer on the spot"
      ] )
  , ( "Watching a run"
    , [ "  space          release the adventurer, pause or resume"
      , "  ← →            previous / next step"
      , "  Home End       first / last step"
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
helpScreen page = center $ hLimit 78 $ borderWithLabel (str label) $ padAll 1 $ vBox $
  map line body ++ [str " ", withAttr aDim (str pager)]
  where
    (title, body) = helpPages !! page
    label = " How to play · " ++ title ++ " "
    line "" = str " "
    line l  = withAttr aLine (str l)
    pager = "page " ++ show (page + 1) ++ " of " ++ show (length helpPages)
