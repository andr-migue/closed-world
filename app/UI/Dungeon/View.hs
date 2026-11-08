module UI.Dungeon.View
  ( RoomState (..)
  , RoomView (..)
  , MapView (..)
  , mapView
  , currentRoom
  ) where

import Data.List (find)
import qualified Data.Text as T
import Protocol

data RoomState = Current | OnPath | Abandoned
  deriving Eq

data RoomView = RoomView
  { rvId     :: Int
  , rvParent :: Maybe Int
  , rvRow    :: Int
  , rvCol    :: Int
  , rvLabel  :: T.Text
  , rvKind   :: T.Text
  , rvGhost  :: Bool
  , rvState  :: RoomState
  , rvDoors  :: [Door]
  }

data MapView = MapView
  { mvRooms   :: [RoomView]
  , mvLastRow :: Int
  , mvLastCol :: Int
  }

mapView :: Run -> Int -> MapView
mapView run k = MapView views lastRow lastCol
  where
    rooms   = filter ((<= k) . roomStep) (runRooms run)
    here    = currentRoom run k
    path    = ancestors run here
    views   = map view rooms
    lastRow = maximum (0 : map roomRow rooms)
    lastCol = maximum (0 : map roomCol (runRooms run))
    view r = RoomView
      { rvId     = roomId r
      , rvParent = roomParent r
      , rvRow    = roomRow r
      , rvCol    = roomCol r
      , rvLabel  = roomLabel r
      , rvKind   = if roomKindStep r <= k then roomKind r else T.pack "room"
      , rvGhost  = roomGhost r
      , rvState  = state (roomId r)
      , rvDoors  = [d | d <- runDoors run, doorRoom d == roomId r, doorStep d <= k]
      }
    state i
      | Just i == here = Current
      | i `elem` path  = OnPath
      | otherwise      = Abandoned

currentRoom :: Run -> Int -> Maybe Int
currentRoom _ 0   = Just 0
currentRoom run k = stepRoom (runSteps run !! (k - 1))

ancestors :: Run -> Maybe Int -> [Int]
ancestors run = go
  where
    go Nothing  = []
    go (Just i) = i : go (roomParent =<< find ((== i) . roomId) (runRooms run))
