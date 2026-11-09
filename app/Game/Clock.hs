module Game.Clock
  ( frameMillis
  , clock
  , elapsed
  , speeds
  , stepFrames
  , progress
  ) where

import Game.State (Game (..))

frameMillis :: Int
frameMillis = 33

clock :: Game -> Int
clock game = frame game * frameMillis `div` 60

elapsed :: Game -> Int -> Int
elapsed game since = (frame game - since) * frameMillis

speeds :: [Int]
speeds = [1080, 660, 360, 180]

stepFrames :: Game -> Int
stepFrames game = (speeds !! max 0 (min (length speeds - 1) (speed game))) `div` frameMillis

progress :: Game -> Double
progress game = max 0 (min 1 (fromIntegral (frame game - stepStart game) / fromIntegral (stepFrames game)))
