module Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Tui.Draw (draw)
import Tui.Events (handle)
import Tui.Names (Name)
import Game.Clock (frameMillis)
import Game.State
import Prolog.Protocol
import Prolog.Server
import Style.Theme (theme)

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
  , appStartEvent   = enableMouse
  , appAttrMap      = const theme
  }

enableMouse :: EventM Name Game ()
enableMouse = do
  vty <- getVtyHandle
  liftIO (V.setMode (V.outputIface vty) V.Mouse True)
