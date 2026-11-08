module State
  ( Screen (..)
  , Game (..)
  , Tick (..)
  , MenuItem (..)
  , menuItems
  , menuLabel
  , totalSteps
  , finished
  , succeeded
  , hasNextLevel
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Editor
import Protocol
import Server

data Screen = Menu Int | Levels Int | Help Int Screen | Playing

data Game = Game
  { server  :: Server
  , levels  :: [LevelSummary]
  , screen  :: Screen
  , levelId :: Int
  , edits   :: [Edit]
  , current :: Maybe Run
  , stepIx  :: Int
  , mode    :: Mode
  , hint    :: Bool
  , playing :: Bool
  , follow  :: Bool
  , frame   :: Int
  , best    :: M.Map Int Int
  , notice  :: String
  }

data Tick = Tick

data MenuItem = PlayItem | HelpItem | QuitItem
  deriving (Eq, Enum, Bounded)

menuItems :: [MenuItem]
menuItems = [minBound .. maxBound]

menuLabel :: MenuItem -> String
menuLabel PlayItem = "Play"
menuLabel HelpItem = "How to play"
menuLabel QuitItem = "Quit"

totalSteps :: Game -> Int
totalSteps = maybe 0 (length . runSteps) . current

finished :: Game -> Bool
finished game = totalSteps game > 0 && stepIx game == totalSteps game

succeeded :: Game -> Bool
succeeded game = maybe False ((== T.pack "success") . runResult) (current game)

hasNextLevel :: Game -> Bool
hasNextLevel game = levelId game < length (levels game)
