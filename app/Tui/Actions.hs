module Tui.Actions
  ( load
  , rerun
  , applyEdits
  , step
  , play
  , togglePlay
  , replayFromStart
  , changeSpeed
  , panBy
  , recenter
  , dragTo
  , endDrag
  , scrollPanel
  , followCamera'
  , goTo
  , stop
  , toggleHint
  , setMode
  , say
  , io
  ) where

import Brick
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Tui.Names (Name (..))
import Dungeon.Camera (clampPan, followCamera)
import Dungeon.Layout (layout)
import Dungeon.Scene (sceneFocus)
import Game.Clock
import Game.Editor
import Game.RunInfo
import Game.State
import Panels.Chronicle (chronicleContent)
import Panels.Program (programContent)
import Panels.Query (queryContent)
import Prolog.Protocol
import Prolog.Server (call)
import Widgets.Scroll (Content, normalize)

load :: Int -> Game -> IO Game
load n game = do
  reply <- call (server game) (runRequest n [])
  pure $ case reply of
    Right run -> fresh game { screen = Playing, levelId = n, edits = [], current = Just run, mode = Replay, hint = False, playing = False }
    Left err  -> game { notice = err }

rerun :: [Edit] -> Game -> IO Game
rerun newEdits game = do
  reply <- call (server game) (runRequest (levelId game) newEdits)
  pure $ case reply of
    Right run -> fresh game { edits = newEdits, current = Just run }
    Left err  -> game { notice = err }

applyEdits :: Cursor -> [Edit] -> EventM Name Game ()
applyEdits cursor newEdits = do
  game <- get
  game' <- liftIO (rerun newEdits game)
  put game' { mode = Editing (fitCursor game' cursor) Nothing }

fresh :: Game -> Game
fresh game = recentered game { stepIx = 0, stepStart = -1000, camera = Nothing, notice = "" }

following :: Game -> Game
following game = game { scrolls = M.empty }

recentered :: Game -> Game
recentered game = following game { pan = (0, 0), drag = Nothing }

fitCursor :: Game -> Cursor -> Cursor
fitCursor game cursor = case current game of
  Just run -> moveCursor (runLevel run) 0 0 cursor
  Nothing  -> cursor

step :: Bool -> (Int -> Int) -> EventM Name Game ()
step animate f = modify $ \game ->
  let k = max 0 (min (totalSteps game) (f (stepIx game)))
      changed = k /= stepIx game
      arrived = changed && k == totalSteps game
      leaving = if stepIx game == 0 && k > 0 then recentered else following
  in rememberStars (leaving game)
       { stepIx    = k
       , playing   = playing game && k < totalSteps game
       , stepStart = if changed then (if animate then frame game else frame game - 1000) else stepStart game
       , endFrame  = if arrived then frame game + (if animate then stepFrames game else 0) else endFrame game
       }

rememberStars :: Game -> Game
rememberStars game
  | finished game && succeeded game, Just run <- current game =
      game { best = M.insertWith max (levelId game) (runStars run) (best game) }
  | otherwise = game

play :: EventM Name Game ()
play = modify $ \g -> recentered g { mode = Replay, stepIx = 0, playing = True, notice = "", stepStart = frame g }

replayFromStart :: EventM Name Game ()
replayFromStart = modify $ \g -> recentered g { stepIx = 0, playing = True, stepStart = frame g }

togglePlay :: EventM Name Game ()
togglePlay = modify $ \g ->
  (if stepIx g == 0 && not (playing g) then recentered g else g)
    { playing = not (playing g)
    , stepStart = if playing g then stepStart g else min (stepStart g) (frame g - stepFrames g) }

changeSpeed :: Int -> EventM Name Game ()
changeSpeed d = modify $ \g -> g { speed = max 0 (min (length speeds - 1) (speed g + d)) }

panBy :: Int -> Int -> EventM Name Game ()
panBy dx dy = do
  extent <- lookupExtent DungeonView
  modify $ \g -> case (current g, extent) of
    (Just run, Just ext) ->
      let (x, y) = pan g
          focus = fromMaybe (sceneFocus run (stepIx g) 1) (camera g)
          moved = clampPan (layout run) (dungeonView ext) focus (x + dx, y + dy)
      in if moved == pan g then g else g { pan = moved, viewMoved = True }
    _ -> g

recenter :: EventM Name Game ()
recenter = modify $ \g -> g { pan = (0, 0), viewMoved = True }

dragTo :: (Int, Int) -> EventM Name Game ()
dragTo (c, r) = do
  game <- get
  put game { drag = Just (c, r) }
  case drag game of
    Just (c0, r0) -> panBy (c0 - c) (2 * (r0 - r))
    Nothing       -> pure ()

endDrag :: EventM Name Game ()
endDrag = modify $ \g -> g { drag = Nothing }

dungeonView :: Extent Name -> (Int, Int)
dungeonView ext = let (w, h) = extentSize ext in (w - 2, 2 * (h - 2))

followCamera' :: EventM Name Game ()
followCamera' = do
  extent <- lookupExtent DungeonView
  modify $ \g -> case (screen g, current g, extent) of
    (Playing, Just run, Just ext) | pan g == (0, 0) || camera g == Nothing ->
      let p = if stepIx g == 0 then 1 else progress g
          target = sceneFocus run (stepIx g) p
      in g { camera = Just (followCamera (dungeonView ext) target (camera g)) }
    _ -> g

scrollPanel :: Name -> Int -> Int -> EventM Name Game ()
scrollPanel n dx dy = do
  game <- get
  extent <- lookupExtent n
  case (current game, extent) of
    (Just run, Just ext) -> do
      let (w, h) = extentSize ext
          innerW = w - 4
          innerH = h - 2
          (ox, oy) = scrollOf n game
          delta = maybe (0, 0) (\c -> normalize c innerW innerH (ox + dx, oy + dy)) (contentOf n game run innerW)
      put game { scrolls = M.insert n delta (scrolls game) }
    _ -> pure ()

contentOf :: Name -> Game -> LevelRun -> Int -> Maybe Content
contentOf n game run w = case n of
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
