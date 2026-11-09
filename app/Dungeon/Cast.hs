module Dungeon.Cast
  ( Cast (..)
  , castAt
  , focusOf
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Dungeon.Layout
import Dungeon.Timeline
import Dungeon.Walking
import Prolog.Protocol

data Cast = Cast
  { castHero  :: Maybe Actor
  , castGhost :: Maybe Actor
  }

castAt :: Layout -> Run -> Int -> Double -> Cast
castAt ly run k p
  | k == 0 = Cast (stand <$> room 0) Nothing
  | otherwise = Cast hero ghost
  where
    room i = M.lookup i (lyRooms ly)
    stand r = let (x, y) = standPoint r in Actor x y FaceDown 0 1
    event = eventAt run k
    walked = min 1 (p / 0.85)
    stepDoor = listToMaybe [d | d <- runDoors run, doorStep d == k]
    heroFrom = heroRoom ly run (k - 1)
    heroTo = heroRoom ly run k
    ghostFrom = ghostRoom ly run (k - 1)
    ghostTo = ghostRoom ly run k
    heroBase = case (heroFrom, heroTo) of
      (Just a, Just b)
        | isEvent "mismatch" event, Nothing <- ghostTo, Just d <- stepDoor, Just r <- room b -> Just (walk (bump ly r d) walked)
        | otherwise -> Just (walk (route ly a b) walked)
      (_, Just b) -> stand <$> room b
      _ -> Nothing
    hero = case heroBase of
      Just a | isEvent "trap" event -> Just a { actorAlpha = dying }
      other -> other
    dying
      | p > 0.6 = 0
      | odd (floor (p * 12) :: Int) = 0.3
      | otherwise = 1
    ghost = case (ghostFrom, ghostTo) of
      (_, Just c)
        | isEvent "mismatch" event, Just d <- stepDoor, Just r <- room c -> Just (walk (bump ly r d) walked)
      (Just a, Just c) -> Just (walk (route ly a c) walked)
      (Nothing, Just c) -> case heroTo of
        Just h -> Just ((walk (route ly h c) walked) { actorAlpha = min 1 (0.3 + walked) })
        Nothing -> stand <$> room c
      (Just a, Nothing)
        | isEvent "not_fails" event -> (\r -> (stand r) { actorAlpha = 1 - walked }) <$> room a
        | Just h <- heroTo -> Just ((walk (route ly a h) walked) { actorAlpha = 1 - walked })
      _ -> Nothing

heroRoom :: Layout -> Run -> Int -> Maybe Int
heroRoom ly run k = listToMaybe [roomId r | r <- maybe [] (ancestry ly) (lastKnownRoom run k), not (roomGhost r)]

ghostRoom :: Layout -> Run -> Int -> Maybe Int
ghostRoom ly run k = case lastKnownRoom run k >>= \i -> M.lookup i (lyRooms ly) of
  Just r | roomGhost r -> Just (roomId r)
  _ -> Nothing

focusOf :: Layout -> Cast -> (Double, Double)
focusOf ly cast = maybe middle (\a -> (actorX a, actorY a - 6)) watched
  where
    middle = (fromIntegral (lyWidth ly) / 2, fromIntegral (lyHeight ly) / 2)
    watched = case castGhost cast of
      Just g | actorAlpha g > 0.3 -> Just g
      _ -> castHero cast
