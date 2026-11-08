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
import Protocol
import Server
import UI.Dungeon
import UI.Panels
import UI.Theme

data Game = Game
  { server  :: Server
  , levels  :: [LevelSummary]
  , current :: Maybe Run
  , stepIx  :: Int
  , notice  :: String
  }

main :: IO ()
main = do
  srv <- start
  summaries <- either (const []) id <$> call srv levelsRequest
  game <- load 1 (Game srv summaries Nothing 0 "")
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
  reply <- call (server game) (runRequest n)
  pure $ case reply of
    Right run -> game { current = Just run, stepIx = 0, notice = "" }
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
               , hLimit 46 (vBox [programPanel run (stepIx game), statusPanel run (stepIx game)])
               ]
        , hBorder
        , if null (notice game) then helpLine else padLeftRight 1 (withAttr aTrap (str (notice game)))
        ]

handle :: BrickEvent Name e -> EventM Name Game ()
handle (VtyEvent (V.EvKey key _)) = case key of
  V.KChar 'q'  -> halt
  V.KEsc       -> halt
  V.KRight     -> moveTo (+ 1)
  V.KLeft      -> moveTo (subtract 1)
  V.KHome      -> moveTo (const 0)
  V.KEnd       -> moveTo (const maxBound)
  V.KChar c | isDigit c, c /= '0' -> do
    game <- get
    game' <- liftIO (load (digitToInt c) game)
    put game'
  _ -> pure ()
handle _ = pure ()

moveTo :: (Int -> Int) -> EventM Name Game ()
moveTo f = modify $ \game -> case current game of
  Nothing  -> game
  Just run -> game { stepIx = max 0 (min (length (runSteps run)) (f (stepIx game))) }
