module Game.RunInfo
  ( totalSteps
  , finished
  , succeeded
  , hasNextLevel
  , torchUsed
  , torchLeft
  , torchRatio
  ) where

import qualified Data.Text as T
import Game.State (Game (..))
import Prolog.Protocol

totalSteps :: Game -> Int
totalSteps = maybe 0 (length . runSteps) . current

finished :: Game -> Bool
finished game = totalSteps game > 0 && stepIx game == totalSteps game

succeeded :: Game -> Bool
succeeded game = maybe False ((== T.pack "success") . runResult) (current game)

hasNextLevel :: Game -> Bool
hasNextLevel game = levelId game < length (levels game)

torchUsed :: LevelRun -> Int -> Int
torchUsed run k = length [s | s <- take k (runSteps run), stepEvent s `notElem` free]
  where free = [T.pack "success", T.pack "trap"]

torchLeft :: LevelRun -> Int -> Int
torchLeft run k = levelTorch (runLevel run) - torchUsed run k

torchRatio :: LevelRun -> Int -> Double
torchRatio run k = fromIntegral (max 0 (torchLeft run k)) / fromIntegral (max 1 (levelTorch (runLevel run)))
