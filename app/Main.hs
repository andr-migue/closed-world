module Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, modify)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Editor
import Protocol
import Server
import State
import UI.Help
import UI.Menu
import UI.Play
import UI.Theme

main :: IO ()
main = do
  srv <- start
  reply <- call srv levelsRequest
  let (summaries, problem) = case reply of
        Right (LevelList l) -> (l, "")
        Left e              -> ([], "Could not start the Prolog server: " ++ e)
  ticks <- newBChan 4
  void (forkIO (forever (writeBChan ticks Tick >> threadDelay 450000)))
  let game = Game srv summaries (Menu 0) 1 [] Nothing 0 Replay False False True 0 0 M.empty problem
  void (customMainWithDefaultVty (Just ticks) app game)

app :: App Game Tick Name
app = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handle
  , appStartEvent   = pure ()
  , appAttrMap      = const theme
  }

draw :: Game -> [Widget Name]
draw game = map (whenFits tooSmall) $ case screen game of
  Menu i     -> [window "" (menuScreen game i <=> noticeLine game)]
  Levels i   -> [window "" (levelsScreen game i <=> keys [("↑/↓", "choose"), ("enter", "play"), ("esc", "menu")])]
  Help p _   -> [window "" (helpScreen p <=> keys [("←/→", "page"), ("esc", "back")])]
  Playing    -> case current game of
    Nothing  -> [window "" (noticeLine game)]
    Just run ->
      [resultDialog game run | finished game, Replay <- [mode game]]
      ++ [window (levelTitle game run) (playScreen game run)]
  where
    window title body = withBorderStyle unicode (borderWithLabel (withAttr aTitle (str (" CLOSED WORLD" ++ title ++ " "))) body)

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

levelTitle :: Game -> Run -> String
levelTitle game run =
  " · " ++ show (levelId game) ++ "/" ++ show (length (levels game)) ++ " · " ++ T.unpack (levelName (runLevel run))

noticeLine :: Game -> Widget Name
noticeLine game
  | null (notice game) = keys [("↑/↓", "choose"), ("enter", "select")]
  | otherwise          = padLeftRight 1 (withAttr aTrap (str (notice game)))

keys :: [(String, String)] -> Widget Name
keys pairs = padLeftRight 1 $ hBox
  [withAttr aKey (str (" " ++ k ++ " ")) <+> str (" " ++ label ++ "   ") | (k, label) <- pairs]

handle :: BrickEvent Name Tick -> EventM Name Game ()
handle (AppEvent Tick) = tick
handle (VtyEvent (V.EvKey key _)) = do
  game <- get
  case screen game of
    Menu i      -> menuKey i key
    Levels i    -> levelsKey i key
    Help p back -> helpKey p back key
    Playing     -> case (mode game, current game) of
      (Editing cursor pick, Just run) -> editKey (runLevel run) cursor pick key
      _                               -> replayKey key
handle _ = pure ()

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

replayKey :: V.Key -> EventM Name Game ()
replayKey key = do
  game <- get
  case key of
    V.KChar ' '
      | finished game -> modify $ \g -> g { stepIx = 0, playing = True }
      | otherwise     -> modify $ \g -> g { playing = not (playing g) }
    V.KRight    -> stop >> moveTo (+ 1)
    V.KLeft     -> stop >> moveTo (subtract 1)
    V.KHome     -> stop >> moveTo (const 0)
    V.KEnd      -> stop >> moveTo (const maxBound)
    V.KChar 'e' -> modify $ \g -> g { mode = Editing startCursor Nothing, playing = False, notice = "" }
    V.KChar 'h' -> toggleHint
    V.KChar 'w' -> pan 0 (-3)
    V.KChar 's' -> pan 0 3
    V.KChar 'a' -> pan (-8) 0
    V.KChar 'd' -> pan 8 0
    V.KChar '?' -> stop >> goTo (Help 0 Playing)
    V.KChar 'n' | finished game, succeeded game, hasNextLevel game -> io (load (levelId game + 1))
    V.KEsc      -> stop >> goTo (Levels (levelId game - 1))
    _           -> pure ()

editKey :: Level -> Cursor -> Maybe Pick -> V.Key -> EventM Name Game ()
editKey level cursor pick key = case key of
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
  V.KEnter    -> modify $ \g -> g { mode = Replay, stepIx = 0, playing = True, notice = "" }
  V.KEsc      -> case pick of
    Just _  -> setMode (Editing cursor Nothing)
    Nothing -> setMode Replay
  _ -> pure ()
  where
    moveCursorBy dr dc = setMode (Editing (moveCursor level dr dc cursor) pick)
    applyEdit edit = do
      game <- get
      game' <- liftIO (rerun (edits game ++ [edit]) game)
      put game' { mode = Editing (fitCursor game' cursor) Nothing }

load :: Int -> Game -> IO Game
load n game = do
  reply <- call (server game) (runRequest n [])
  pure $ case reply of
    Right run -> game { screen = Playing, levelId = n, edits = [], current = Just run, stepIx = 0
                      , mode = Replay, hint = False, playing = False, follow = True, notice = "" }
    Left err  -> game { notice = err }

rerun :: [Edit] -> Game -> IO Game
rerun newEdits game = do
  reply <- call (server game) (runRequest (levelId game) newEdits)
  pure $ case reply of
    Right run -> game { edits = newEdits, current = Just run, stepIx = 0, follow = True, notice = "" }
    Left err  -> game { notice = err }

tick :: EventM Name Game ()
tick = do
  modify $ \g -> g { frame = frame g + 1 }
  game <- get
  case (screen game, mode game) of
    (Playing, Replay) | playing game ->
      if stepIx game < totalSteps game
        then moveTo (+ 1)
        else stop
    _ -> pure ()

moveTo :: (Int -> Int) -> EventM Name Game ()
moveTo f = modify $ \game ->
  let k = max 0 (min (totalSteps game) (f (stepIx game)))
      arrived = k == totalSteps game && stepIx game /= k
  in record game { stepIx = k, playing = playing game && k < totalSteps game, follow = True
                 , endFrame = if arrived then frame game else endFrame game }

record :: Game -> Game
record game
  | finished game && succeeded game, Just run <- current game =
      game { best = M.insertWith max (levelId game) (runStars run) (best game) }
  | otherwise = game

fitCursor :: Game -> Cursor -> Cursor
fitCursor game cursor = case current game of
  Just run -> moveCursor (runLevel run) 0 0 cursor
  Nothing  -> cursor

pan :: Int -> Int -> EventM Name Game ()
pan dx dy = do
  modify $ \g -> g { follow = False }
  let vp = viewportScroll DungeonView
  hScrollBy vp dx
  vScrollBy vp dy

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
