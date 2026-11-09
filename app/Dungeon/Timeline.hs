module Dungeon.Timeline
  ( RoomState (..)
  , RoomView (..)
  , roomsAt
  , currentRoom
  , lastKnownRoom
  , eventAt
  , isEvent
  ) where

import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Prolog.Protocol

data RoomState = Current | OnPath | Abandoned
  deriving Eq

data RoomView = RoomView
  { rvId     :: Int
  , rvParent :: Maybe Int
  , rvRow    :: Int
  , rvCol    :: Int
  , rvKind   :: T.Text
  , rvGhost  :: Bool
  , rvState  :: RoomState
  }

roomsAt :: Run -> Int -> [RoomView]
roomsAt run k = map view (filter ((<= k) . roomStep) (runRooms run))
  where
    here = currentRoom run k
    path = ancestors run here
    view r = RoomView
      { rvId     = roomId r
      , rvParent = roomParent r
      , rvRow    = roomRow r
      , rvCol    = roomCol r
      , rvKind   = if roomKindStep r <= k then roomKind r else T.pack "room"
      , rvGhost  = roomGhost r
      , rvState  = state (roomId r)
      }
    state i
      | Just i == here = Current
      | i `elem` path  = OnPath
      | otherwise      = Abandoned

currentRoom :: Run -> Int -> Maybe Int
currentRoom _ 0   = Just 0
currentRoom run k = stepRoom (runSteps run !! (k - 1))

lastKnownRoom :: Run -> Int -> Maybe Int
lastKnownRoom run k = listToMaybe (mapMaybe (currentRoom run) [k, k - 1 .. 0])

eventAt :: Run -> Int -> Maybe Step
eventAt run k
  | k >= 1 && k <= length (runSteps run) = Just (runSteps run !! (k - 1))
  | otherwise = Nothing

isEvent :: String -> Maybe Step -> Bool
isEvent name = maybe False ((== T.pack name) . stepEvent)

ancestors :: Run -> Maybe Int -> [Int]
ancestors run = go
  where
    go Nothing  = []
    go (Just i) = i : go (roomParent =<< find ((== i) . roomId) (runRooms run))
