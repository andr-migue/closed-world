module Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Editor
import Protocol
import Server
import State
import UI.Backdrop (backdrop)
import UI.Help
import UI.Menu
import UI.Panels (chronicleContent, programContent, queryContent)
import UI.Play
import UI.Dungeon.Scene (followCamera, sceneFocus)
import UI.Scroll (Content, normalize)
import UI.Theme

main :: IO ()
main = do
  srv <- start
  reply <- call srv levelsRequest
  let (summaries, problem) = case reply of
        Right (LevelList l) -> (l, "")
        Left e              -> ([], "Could not start the Prolog server: " ++ e)
  ticks <- newBChan 4
  void (forkIO (forever (writeBChan ticks Tick >> threadDelay (frameMillis * 1000))))
  void (customMainWithDefaultVty (Just ticks) app (newGame srv summaries problem))

app :: App Game Tick Name
app = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handle
  , appStartEvent   = do
      vty <- getVtyHandle
      liftIO (V.setMode (V.outputIface vty) V.Mouse True)
  , appAttrMap      = const theme
  }

draw :: Game -> [Widget Name]
draw game = map (whenFits tooSmall) $ case screen game of
  Menu i     -> [ bottomRow (keyBar [("↑/↓", "choose"), ("enter", "select"), ("q", "quit")])
                , lowered (menuScreen game i <=> noticeLine game)
                , backdrop True (clock game) ]
  Levels i   -> scenic game True (levelsScreen game i) [("↑/↓", "choose"), ("enter", "play"), ("esc", "menu")]
  Help p _   -> scenic game True (helpScreen game p) [("←/→", "page"), ("esc", "back")]
  Playing    -> case current game of
    Nothing  -> [noticeLine game]
    Just run ->
      [resultDialog game run | finished game, Replay <- [mode game], elapsed game (endFrame game) > 480]
      ++ [playScreen game run]

scenic :: Game -> Bool -> Widget Name -> [(String, String)] -> [Widget Name]
scenic game withHeader body pairs =
  [ bottomRow (keyBar pairs) ]
  ++ [ header game Nothing | withHeader ]
  ++ [ centerLayer body, backdrop False (clock game) ]

lowered :: Widget Name -> Widget Name
lowered w = Widget Greedy Greedy $ do
  ctx <- getContext
  render (translateBy (Location ((availWidth ctx - 40) `div` 2, availHeight ctx * 52 `div` 100)) w)

bottomRow :: Widget Name -> Widget Name
bottomRow w = Widget Greedy Greedy $ do
  ctx <- getContext
  render (translateBy (Location (0, availHeight ctx - 1)) (vLimit 1 (hLimit (availWidth ctx) w)))

minWidth, minHeight :: Int
minWidth  = 120
minHeight = 34

whenFits :: Widget Name -> Widget Name -> Widget Name
whenFits fallback w = Widget Greedy Greedy $ do
  ctx <- getContext
  if availWidth ctx < minWidth || availHeight ctx < minHeight
    then render fallback
    else render w

tooSmall :: Widget Name
tooSmall = Widget Greedy Greedy $ do
  ctx <- getContext
  render $ center $ vBox
    [ hCenter (withAttr aTitle (str "The terminal is too small"))
    , str " "
    , hCenter (withAttr aLine (str ("Closed World needs at least " ++ show minWidth ++ " × " ++ show minHeight ++ ".")))
    , hCenter (withAttr aDim (str ("Now it is " ++ show (availWidth ctx) ++ " × " ++ show (availHeight ctx) ++ ".")))
    ]

noticeLine :: Game -> Widget Name
noticeLine game
  | null (notice game) = emptyWidget
  | otherwise          = padLeftRight 1 (withAttr aTrap (str (notice game)))

handle :: BrickEvent Name Tick -> EventM Name Game ()
handle (AppEvent Tick) = tick
handle (MouseDown n button mods _) = invalidateCache >> case button of
  V.BScrollUp   -> wheel n mods (-1)
  V.BScrollDown -> wheel n mods 1
  _             -> pure ()
handle (VtyEvent (V.EvKey key mods)) = do
  invalidateCache
  game <- get
  case screen game of
    Menu i      -> menuKey i key
    Levels i    -> levelsKey i key
    Help p back -> helpKey p back key
    Playing     -> case (mode game, current game) of
      (Editing cursor pick, Just run) -> editKey (runLevel run) cursor pick key mods
      _                               -> replayKey key mods
handle (VtyEvent _) = invalidateCache
handle _ = pure ()

wheel :: Name -> [V.Modifier] -> Int -> EventM Name Game ()
wheel n mods d
  | n == DungeonView = if V.MShift `elem` mods then pan' (8 * d) 0 else pan' 0 (6 * d)
  | V.MShift `elem` mods = scrollPanel n (4 * d) 0
  | otherwise = scrollPanel n 0 (2 * d)

menuKey :: Int -> V.Key -> EventM Name Game ()
menuKey i key = case key of
  V.KUp    -> goTo (Menu (max 0 (i - 1)))
  V.KDown  -> goTo (Menu (min (length menuItems - 1) (i + 1)))
  V.KEnter -> case menuItems !! i of
    PlayItem -> do
      game <- get
      goTo (Levels (firstUnsolved game))
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
  V.KRight -> goTo (Help (min (length helpPages - 1) (p + 1)) back)
  V.KLeft  -> goTo (Help (max 0 (p - 1)) back)
  V.KEsc   -> goTo back
  V.KChar '?' -> goTo back
  _        -> pure ()

replayKey :: V.Key -> [V.Modifier] -> EventM Name Game ()
replayKey key _ = do
  game <- get
  case key of
    V.KChar ' '
      | finished game -> modify $ \g -> follow g { stepIx = 0, playing = True, stepStart = frame g }
      | otherwise     -> modify $ \g -> g { playing = not (playing g), stepStart = if playing g then stepStart g else min (stepStart g) (frame g - stepFrames g) }
    V.KRight    -> stop >> moveTo True (+ 1)
    V.KLeft     -> stop >> moveTo False (subtract 1)
    V.KHome     -> stop >> moveTo False (const 0)
    V.KEnd      -> stop >> moveTo False (const maxBound)
    V.KChar '+' -> changeSpeed 1
    V.KChar '=' -> changeSpeed 1
    V.KChar '-' -> changeSpeed (-1)
    V.KChar 'm' -> modify $ \g -> g { minimap = not (minimap g) }
    V.KChar 'f' -> modify $ \g -> g { fluid = not (fluid g), notice = if fluid g then "Eco mode: 15 fps, lighter on the CPU." else "Fluid mode: 30 fps while the adventurer walks." }
    V.KChar 'e' -> modify $ \g -> g { mode = Editing startCursor Nothing, playing = False, notice = "", scrolls = M.empty }
    V.KChar 'h' -> toggleHint
    V.KChar 'w' -> pan' 0 (-6)
    V.KChar 's' -> pan' 0 6
    V.KChar 'a' -> pan' (-10) 0
    V.KChar 'd' -> pan' 10 0
    V.KPageUp   -> scrollPanel ProgramView 0 (-3)
    V.KPageDown -> scrollPanel ProgramView 0 3
    V.KChar '[' -> scrollPanel ProgramView (-6) 0
    V.KChar ']' -> scrollPanel ProgramView 6 0
    V.KChar '?' -> stop >> goTo (Help 0 Playing)
    V.KChar 'n' | finished game, succeeded game, hasNextLevel game -> io (load (levelId game + 1))
    V.KEsc      -> stop >> goTo (Levels (levelId game - 1))
    _           -> pure ()

editKey :: Level -> Cursor -> Maybe Pick -> V.Key -> [V.Modifier] -> EventM Name Game ()
editKey level cursor pick key _ = case key of
  V.KUp       -> moveCursorBy (-1) 0
  V.KDown     -> moveCursorBy 1 0
  V.KLeft     -> moveCursorBy 0 (-1)
  V.KRight    -> moveCursorBy 0 1
  V.KChar ' ' -> case pickOrDrop level cursor pick of
    Left err           -> say err
    Right (Left p)     -> setMode (Editing cursor (Just p))
    Right (Right edit) -> applyEdit edit
  V.KChar 'c' -> either say applyEdit (cutAt level cursor)
  V.KChar 'x' -> either say applyEdit (uncutAt level cursor)
  V.KChar 'u' -> do
    game <- get
    if null (edits game) then say "Nothing to undo." else io (rerun (init (edits game)))
  V.KChar 'R' -> io (rerun [])
  V.KChar 'h' -> toggleHint
  V.KPageUp   -> scrollPanel ProgramView 0 (-3)
  V.KPageDown -> scrollPanel ProgramView 0 3
  V.KChar '[' -> scrollPanel ProgramView (-6) 0
  V.KChar ']' -> scrollPanel ProgramView 6 0
  V.KEnter    -> modify $ \g -> follow g { mode = Replay, stepIx = 0, playing = True, notice = "", stepStart = frame g }
  V.KEsc      -> case pick of
    Just _  -> setMode (Editing cursor Nothing)
    Nothing -> modify $ \g -> g { mode = Replay, notice = "", scrolls = M.empty }
  _ -> pure ()
  where
    moveCursorBy dr dc = modify $ \g -> g { mode = Editing (moveCursor level dr dc cursor) pick, notice = "", scrolls = M.empty }
    applyEdit edit = do
      game <- get
      game' <- liftIO (rerun (edits game ++ [edit]) game)
      put game' { mode = Editing (fitCursor game' cursor) Nothing }

load :: Int -> Game -> IO Game
load n game = do
  reply <- call (server game) (runRequest n [])
  pure $ case reply of
    Right run -> follow game { screen = Playing, levelId = n, edits = [], current = Just run, stepIx = 0, camera = Nothing
                             , mode = Replay, hint = False, playing = False, notice = "", stepStart = -1000 }
    Left err  -> game { notice = err }

rerun :: [Edit] -> Game -> IO Game
rerun newEdits game = do
  reply <- call (server game) (runRequest (levelId game) newEdits)
  pure $ case reply of
    Right run -> follow game { edits = newEdits, current = Just run, stepIx = 0, notice = "", stepStart = -1000, camera = Nothing }
    Left err  -> game { notice = err }

follow :: Game -> Game
follow g = g { pan = (0, 0), scrolls = M.empty }

tick :: EventM Name Game ()
tick = do
  modify $ \g -> g { frame = frame g + 1 }
  mext <- lookupExtent DungeonView
  modify $ \g -> case (screen g, current g, mext) of
    (Playing, Just run, Just ext) ->
      let (w, h) = extentSize ext
          p = if stepIx g == 0 then 1 else progress g
      in g { camera = Just (followCamera (w - 2, 2 * (h - 2)) (sceneFocus run (stepIx g) p) (camera g)) }
    _ -> g
  game <- get
  case (screen game, mode game) of
    (Playing, Replay) | playing game, frame game - stepStart game >= stepFrames game ->
      if stepIx game < totalSteps game
        then invalidateCache >> moveTo True (+ 1)
        else invalidateCache >> stop
    _ -> do
      let moving = case (screen game, mode game) of
            (Playing, Replay) -> fluid game && (playing game || progress game < 1)
            _ -> False
          ticked = clock game /= clock game { frame = frame game - 1 }
      when (not moving && not ticked) continueWithoutRedraw

moveTo :: Bool -> (Int -> Int) -> EventM Name Game ()
moveTo animate f = modify $ \game ->
  let k = max 0 (min (totalSteps game) (f (stepIx game)))
      arrived = k == totalSteps game && stepIx game /= k
      changed = k /= stepIx game
  in record (follow game)
       { stepIx = k
       , playing = playing game && k < totalSteps game
       , stepStart = if changed then (if animate then frame game else frame game - 1000) else stepStart game
       , endFrame = if arrived then frame game + (if animate then stepFrames game else 0) else endFrame game
       }

changeSpeed :: Int -> EventM Name Game ()
changeSpeed d = modify $ \g -> g { speed = max 0 (min (length speeds - 1) (speed g + d)) }

record :: Game -> Game
record game
  | finished game && succeeded game, Just run <- current game =
      game { best = M.insertWith max (levelId game) (runStars run) (best game) }
  | otherwise = game

fitCursor :: Game -> Cursor -> Cursor
fitCursor game cursor = case current game of
  Just run -> moveCursor (runLevel run) 0 0 cursor
  Nothing  -> cursor

pan' :: Int -> Int -> EventM Name Game ()
pan' dx dy = modify $ \g -> let (x, y) = pan g in g { pan = (x + dx, y + dy) }

scrollPanel :: Name -> Int -> Int -> EventM Name Game ()
scrollPanel n dx dy = do
  game <- get
  mext <- lookupExtent n
  case (current game, mext) of
    (Just run, Just ext) -> do
      let (w, h) = extentSize ext
          iw = w - 4
          ih = h - 2
          content = contentFor n game run iw
          (ox, oy) = scrollOf n game
          delta = maybe (0, 0) (\c -> normalize c iw ih (ox + dx, oy + dy)) content
      put game { scrolls = M.insert n delta (scrolls game) }
    _ -> pure ()

contentFor :: Name -> Game -> Run -> Int -> Maybe Content
contentFor n game run w = case n of
  ProgramView   -> Just (programContent (mode game) run (stepIx game))
  QueryView     -> Just (queryContent (mode game) run)
  ChronicleView -> Just (chronicleContent run (stepIx game) w)
  _             -> Nothing

goTo :: Screen -> EventM Name Game ()
goTo s = modify $ \g -> g { screen = s, notice = "" }

stop :: EventM Name Game ()
stop = modify $ \g -> g { playing = False }

toggleHint :: EventM Name Game ()
toggleHint = modify $ \g -> g { hint = not (hint g) }

setMode :: Mode -> EventM Name Game ()
setMode m = modify $ \g -> g { mode = m, notice = "" }

say :: String -> EventM Name Game ()
say message = modify $ \g -> g { notice = message }

io :: (Game -> IO Game) -> EventM Name Game ()
io f = get >>= liftIO . f >>= put
