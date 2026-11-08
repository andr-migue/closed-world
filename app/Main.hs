module Main (main) where

import Brick
import Brick.Widgets.Border (hBorder, vBorder, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, modify)
import Data.Char (digitToInt, isDigit)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Editor
import Protocol
import Server
import UI.Dungeon
import UI.Panels
import UI.Theme

data Game = Game
  { server  :: Server
  , levels  :: [LevelSummary]
  , levelId :: Int
  , edits   :: [Edit]
  , current :: Maybe Run
  , stepIx  :: Int
  , mode    :: Mode
  , hint    :: Bool
  , notice  :: String
  }

main :: IO ()
main = do
  srv <- start
  summaries <- either (const []) id <$> call srv levelsRequest
  game <- load 1 (Game srv summaries 1 [] Nothing 0 Replay False "")
  void (defaultMain app game)

app :: App Game e Name
app = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handle
  , appStartEvent   = pure ()
  , appAttrMap      = const theme
  }

load :: Int -> Game -> IO Game
load n game = do
  reply <- call (server game) (runRequest n [])
  pure $ case reply of
    Right run -> game { levelId = n, edits = [], current = Just run, stepIx = 0, mode = Replay, hint = False, notice = "" }
    Left err  -> game { notice = err }

rerun :: [Edit] -> Game -> IO Game
rerun newEdits game = do
  reply <- call (server game) (runRequest (levelId game) newEdits)
  pure $ case reply of
    Right run -> game { edits = newEdits, current = Just run, stepIx = 0, notice = "" }
    Left err  -> game { notice = err }

draw :: Game -> [Widget Name]
draw game = [withBorderStyle unicode (borderWithLabel title body)]
  where
    title = withAttr aTitle (txt (T.pack " CLOSED WORLD " <> maybe T.empty name (current game) <> T.pack " "))
    name run = T.pack "· " <> levelName (runLevel run) <> T.pack " "
    body = case current game of
      Nothing  -> padAll 1 (str (notice game))
      Just run -> vBox
        [ hBox [ padAll 1 (dungeon run (stepIx game))
               , vBorder
               , hLimit 52 (vBox [ programPanel (mode game) run (stepIx game)
                                 , statusPanel (hint game) run (stepIx game) ])
               ]
        , hBorder
        , if null (notice game)
            then helpLine (mode game)
            else padLeftRight 1 (withAttr aTrap (str (notice game)))
        ]

handle :: BrickEvent Name e -> EventM Name Game ()
handle (VtyEvent (V.EvKey (V.KChar 'h') [])) = modify $ \g -> g { hint = not (hint g) }
handle (VtyEvent (V.EvKey key mods)) = do
  game <- get
  case (mode game, current game) of
    (Editing cursor pick, Just run) -> editKey (runLevel run) cursor pick key mods
    _                               -> replayKey key
handle _ = pure ()

replayKey :: V.Key -> EventM Name Game ()
replayKey key = case key of
  V.KChar 'q' -> halt
  V.KEsc      -> halt
  V.KRight    -> moveTo (+ 1)
  V.KLeft     -> moveTo (subtract 1)
  V.KHome     -> moveTo (const 0)
  V.KEnd      -> moveTo (const maxBound)
  V.KChar 'e' -> modify $ \g -> g { mode = Editing startCursor Nothing, notice = "" }
  V.KChar c | isDigit c, c /= '0' -> io (load (digitToInt c))
  _ -> pure ()

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
  V.KEnter    -> modify $ \g -> g { mode = Replay, stepIx = 0, notice = "" }
  V.KEsc      -> case pick of
    Just _  -> setMode (Editing cursor Nothing)
    Nothing -> setMode Replay
  V.KChar 'q' -> halt
  _ -> pure ()
  where
    moveCursorBy dr dc = setMode (Editing (moveCursor level dr dc cursor) pick)
    applyEdit edit = do
      game <- get
      game' <- liftIO (rerun (edits game ++ [edit]) game)
      put game' { mode = Editing (fitCursor game' cursor) Nothing }

fitCursor :: Game -> Cursor -> Cursor
fitCursor game cursor = case current game of
  Just run -> moveCursor (runLevel run) 0 0 cursor
  Nothing  -> cursor

setMode :: Mode -> EventM Name Game ()
setMode m = modify $ \g -> g { mode = m, notice = "" }

say :: String -> EventM Name Game ()
say message = modify $ \g -> g { notice = message }

io :: (Game -> IO Game) -> EventM Name Game ()
io f = get >>= liftIO . f >>= put

moveTo :: (Int -> Int) -> EventM Name Game ()
moveTo f = modify $ \game -> case current game of
  Nothing  -> game
  Just run -> game { stepIx = max 0 (min (length (runSteps run)) (f (stepIx game))) }
