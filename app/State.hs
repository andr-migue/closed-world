module State
  ( Screen (..)
  , Game (..)
  , Tick (..)
  , MenuItem (..)
  , newGame
  , menuItems
  , menuLabel
  , totalSteps
  , finished
  , succeeded
  , hasNextLevel
  , stepFrames
  , speeds
  , progress
  , torchUsed
  , torchLeft
  , scrollOf
  , frameMillis
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Editor
import Protocol
import Server
import UI.Theme (Name)

data Screen = Menu Int | Levels Int | Help Int Screen | Playing

data Game = Game
  { server    :: Server
  , levels    :: [LevelSummary]
  , screen    :: Screen
  , levelId   :: Int
  , edits     :: [Edit]
  , current   :: Maybe Run
  , stepIx    :: Int
  , mode      :: Mode
  , hint      :: Bool
  , playing   :: Bool
  , frame     :: Int
  , stepStart :: Int
  , speed     :: Int
  , pan       :: (Int, Int)
  , endFrame  :: Int
  , best      :: M.Map Int Int
  , notice    :: String
  , scrolls   :: M.Map Name (Int, Int)
  , minimap   :: Bool
  }

data Tick = Tick

data MenuItem = PlayItem | HelpItem | QuitItem
  deriving (Eq, Enum, Bounded)

newGame :: Server -> [LevelSummary] -> String -> Game
newGame srv summaries problem = Game
  { server = srv, levels = summaries, screen = Menu 0, levelId = 1, edits = [], current = Nothing
  , stepIx = 0, mode = Replay, hint = False, playing = False, frame = 0, stepStart = -1000
  , speed = 1, pan = (0, 0), endFrame = 0, best = M.empty, notice = problem
  , scrolls = M.empty, minimap = True }

frameMillis :: Int
frameMillis = 60

menuItems :: [MenuItem]
menuItems = [minBound .. maxBound]

menuLabel :: MenuItem -> String
menuLabel PlayItem = "Play"
menuLabel HelpItem = "How to play"
menuLabel QuitItem = "Quit"

speeds :: [Int]
speeds = [18, 11, 6, 3]

stepFrames :: Game -> Int
stepFrames game = speeds !! max 0 (min (length speeds - 1) (speed game))

progress :: Game -> Double
progress game = max 0 (min 1 (fromIntegral (frame game - stepStart game) / fromIntegral (stepFrames game)))

totalSteps :: Game -> Int
totalSteps = maybe 0 (length . runSteps) . current

finished :: Game -> Bool
finished game = totalSteps game > 0 && stepIx game == totalSteps game

succeeded :: Game -> Bool
succeeded game = maybe False ((== T.pack "success") . runResult) (current game)

hasNextLevel :: Game -> Bool
hasNextLevel game = levelId game < length (levels game)

torchUsed :: Run -> Int -> Int
torchUsed run k = length [s | s <- take k (runSteps run), stepEvent s `notElem` free]
  where free = [T.pack "success", T.pack "trap"]

torchLeft :: Run -> Int -> Int
torchLeft run k = levelTorch (runLevel run) - torchUsed run k

scrollOf :: Name -> Game -> (Int, Int)
scrollOf n = M.findWithDefault (0, 0) n . scrolls
