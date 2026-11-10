module Game.State
  ( Screen (..)
  , Game (..)
  , Tick (..)
  , MenuItem (..)
  , newGame
  , menuItems
  , menuLabel
  , scrollOf
  ) where

import qualified Data.Map.Strict as M
import Tui.Names (Name)
import Game.Editor (Mode (..))
import Prolog.Protocol
import Prolog.Server (Server)

data Screen = Menu Int | Levels Int | Help Int Screen | Playing

data Game = Game
  { server    :: Server
  , levels    :: [LevelSummary]
  , screen    :: Screen
  , levelId   :: Int
  , edits     :: [Edit]
  , current   :: Maybe LevelRun
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
  , camera    :: Maybe (Double, Double)
  , fluid     :: Bool
  , drag      :: Maybe (Int, Int)
  , viewMoved :: Bool
  }

data Tick = Tick

data MenuItem = PlayItem | HelpItem | QuitItem
  deriving (Eq, Enum, Bounded)

newGame :: Server -> [LevelSummary] -> String -> Game
newGame srv summaries problem = Game
  { server    = srv
  , levels    = summaries
  , screen    = Menu 0
  , levelId   = 1
  , edits     = []
  , current   = Nothing
  , stepIx    = 0
  , mode      = Replay
  , hint      = False
  , playing   = False
  , frame     = 0
  , stepStart = -1000
  , speed     = 0
  , pan       = (0, 0)
  , endFrame  = 0
  , best      = M.empty
  , notice    = problem
  , scrolls   = M.empty
  , minimap   = True
  , camera    = Nothing
  , fluid     = True
  , drag      = Nothing
  , viewMoved = False
  }

menuItems :: [MenuItem]
menuItems = [minBound .. maxBound]

menuLabel :: MenuItem -> String
menuLabel PlayItem = "Play"
menuLabel HelpItem = "How to play"
menuLabel QuitItem = "Quit"

scrollOf :: Name -> Game -> (Int, Int)
scrollOf n = M.findWithDefault (0, 0) n . scrolls
