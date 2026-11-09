module Tui.Events (handle) where

import Brick
import Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Tui.Actions
import Tui.Names (Name (..))
import Game.Clock
import Game.Editor
import Game.RunInfo
import Game.State
import Prolog.Protocol
import Screens.Help (helpPages)

handle :: BrickEvent Name Tick -> EventM Name Game ()
handle (AppEvent Tick) = tick
handle (MouseDown n button mods _) = invalidateCache >> case button of
  V.BScrollUp   -> wheel n mods (-1)
  V.BScrollDown -> wheel n mods 1
  _             -> pure ()
handle (VtyEvent (V.EvKey key _)) = do
  invalidateCache
  game <- get
  case screen game of
    Menu i      -> menuKey i key
    Levels i    -> levelsKey i key
    Help p back -> helpKey p back key
    Playing     -> case (mode game, current game) of
      (Editing cursor pick, Just run) -> editKey (runLevel run) cursor pick key
      _                               -> replayKey key
handle (VtyEvent _) = invalidateCache
handle _ = pure ()

tick :: EventM Name Game ()
tick = do
  modify $ \g -> g { frame = frame g + 1 }
  followCamera'
  game <- get
  case (screen game, mode game) of
    (Playing, Replay) | playing game, frame game - stepStart game >= stepFrames game ->
      if stepIx game < totalSteps game
        then invalidateCache >> step True (+ 1)
        else invalidateCache >> stop
    _ -> do
      let moving = case (screen game, mode game) of
            (Playing, Replay) -> fluid game && (playing game || progress game < 1)
            _ -> False
          ticked = clock game /= clock game { frame = frame game - 1 }
      when (not moving && not ticked) continueWithoutRedraw

wheel :: Name -> [V.Modifier] -> Int -> EventM Name Game ()
wheel n mods d
  | n == DungeonView = if sideways then panBy (8 * d) 0 else panBy 0 (6 * d)
  | sideways = scrollPanel n (4 * d) 0
  | otherwise = scrollPanel n 0 (2 * d)
  where sideways = V.MShift `elem` mods

menuKey :: Int -> V.Key -> EventM Name Game ()
menuKey i key = case key of
  V.KUp    -> goTo (Menu (max 0 (i - 1)))
  V.KDown  -> goTo (Menu (min (length menuItems - 1) (i + 1)))
  V.KEnter -> case menuItems !! i of
    PlayItem -> get >>= goTo . Levels . firstUnsolved
    HelpItem -> goTo (Help 0 (Menu i))
    QuitItem -> halt
  V.KChar 'q' -> halt
  V.KEsc      -> halt
  _ -> pure ()

firstUnsolved :: Game -> Int
firstUnsolved game =
  case [i | (i, l) <- zip [0 ..] (levels game), M.notMember (summaryId l) (best game)] of
    (i : _) -> i
    []      -> 0

levelsKey :: Int -> V.Key -> EventM Name Game ()
levelsKey i key = do
  game <- get
  let n = length (levels game)
  case key of
    V.KUp    -> goTo (Levels (max 0 (i - 1)))
    V.KDown  -> goTo (Levels (min (n - 1) (i + 1)))
    V.KEnter -> when (n > 0) (io (load (summaryId (levels game !! i))))
    V.KEsc   -> goTo (Menu 0)
    _        -> pure ()

helpKey :: Int -> Screen -> V.Key -> EventM Name Game ()
helpKey p back key = case key of
  V.KRight    -> goTo (Help (min (length helpPages - 1) (p + 1)) back)
  V.KLeft     -> goTo (Help (max 0 (p - 1)) back)
  V.KEsc      -> goTo back
  V.KChar '?' -> goTo back
  _           -> pure ()

replayKey :: V.Key -> EventM Name Game ()
replayKey key = do
  game <- get
  case key of
    V.KChar ' '
      | finished game -> replayFromStart
      | otherwise     -> togglePlay
    V.KRight    -> stop >> step True (+ 1)
    V.KLeft     -> stop >> step False (subtract 1)
    V.KHome     -> stop >> step False (const 0)
    V.KEnd      -> stop >> step False (const maxBound)
    V.KChar '+' -> changeSpeed 1
    V.KChar '=' -> changeSpeed 1
    V.KChar '-' -> changeSpeed (-1)
    V.KChar 'm' -> modify $ \g -> g { minimap = not (minimap g) }
    V.KChar 'f' -> modify toggleFluid
    V.KChar 'e' -> modify $ \g -> g { mode = Editing startCursor Nothing, playing = False, notice = "", scrolls = M.empty }
    V.KChar 'h' -> toggleHint
    V.KChar 'w' -> panBy 0 (-6)
    V.KChar 's' -> panBy 0 6
    V.KChar 'a' -> panBy (-10) 0
    V.KChar 'd' -> panBy 10 0
    V.KChar '?' -> stop >> goTo (Help 0 Playing)
    V.KChar 'n' | finished game, succeeded game, hasNextLevel game -> io (load (levelId game + 1))
    V.KEsc      -> stop >> goTo (Levels (levelId game - 1))
    _           -> programScrollKey key

editKey :: Level -> Cursor -> Maybe Pick -> V.Key -> EventM Name Game ()
editKey level cursor pick key = case key of
  V.KUp       -> moveCursorBy (-1) 0
  V.KDown     -> moveCursorBy 1 0
  V.KLeft     -> moveCursorBy 0 (-1)
  V.KRight    -> moveCursorBy 0 1
  V.KChar ' ' -> case pickOrDrop level cursor pick of
    Left err           -> say err
    Right (Left p)     -> setMode (Editing cursor (Just p))
    Right (Right edit) -> addEdit edit
  V.KChar 'c' -> either say addEdit (cutAt level cursor)
  V.KChar 'x' -> either say addEdit (uncutAt level cursor)
  V.KChar 'u' -> do
    game <- get
    if null (edits game) then say "Nothing to undo." else applyEdits cursor (init (edits game))
  V.KChar 'R' -> applyEdits cursor []
  V.KChar 'h' -> toggleHint
  V.KEnter    -> play
  V.KEsc      -> case pick of
    Just _  -> setMode (Editing cursor Nothing)
    Nothing -> modify $ \g -> g { mode = Replay, notice = "", scrolls = M.empty }
  _ -> programScrollKey key
  where
    moveCursorBy dr dc = modify $ \g -> g { mode = Editing (moveCursor level dr dc cursor) pick, notice = "", scrolls = M.empty }
    addEdit edit = get >>= \game -> applyEdits cursor (edits game ++ [edit])

programScrollKey :: V.Key -> EventM Name Game ()
programScrollKey key = case key of
  V.KPageUp   -> scrollPanel ProgramView 0 (-3)
  V.KPageDown -> scrollPanel ProgramView 0 3
  V.KChar '[' -> scrollPanel ProgramView (-6) 0
  V.KChar ']' -> scrollPanel ProgramView 6 0
  _           -> pure ()

toggleFluid :: Game -> Game
toggleFluid g = g { fluid = not (fluid g), notice = message }
  where
    message
      | fluid g   = "Eco mode: 15 fps, lighter on the CPU."
      | otherwise = "Fluid mode: 30 fps while the adventurer walks."
