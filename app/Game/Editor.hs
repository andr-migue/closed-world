module Game.Editor
  ( Cursor (..)
  , Pick (..)
  , Mode (..)
  , startCursor
  , moveCursor
  , rowGoals
  , isQueryRow
  , pickOrDrop
  , cutAt
  , uncutAt
  ) where

import qualified Data.Text as T
import Prolog.Protocol

data Cursor = Cursor { curRow :: Int, curCol :: Int }
  deriving Eq

data Pick = PickClause Int | PickGoal Int Int

data Mode = Replay | Editing Cursor (Maybe Pick)

-- Rows 1..n are clauses and row n+1 is the query.
-- Column 0 is a clause head, 1..k its goals and k+1 the end of the body.

clauseCount :: Level -> Int
clauseCount = length . levelClauses

isQueryRow :: Level -> Int -> Bool
isQueryRow level row = row > clauseCount level

rowGoals :: Level -> Int -> [T.Text]
rowGoals level row
  | isQueryRow level row = levelQueryGoals level
  | otherwise            = clauseGoals (levelClauses level !! (row - 1))

target :: Level -> Int -> Target
target level row
  | isQueryRow level row = Query
  | otherwise            = Clause row

minCol, maxCol :: Level -> Int -> Int
minCol level row = if isQueryRow level row then 1 else 0
maxCol level row = length (rowGoals level row) + 1

startCursor :: Cursor
startCursor = Cursor 1 0

moveCursor :: Level -> Int -> Int -> Cursor -> Cursor
moveCursor level dRow dCol (Cursor row col) = Cursor row' col'
  where
    row' = clamp 1 (clauseCount level + 1) (row + dRow)
    col' = clamp (minCol level row') (maxCol level row') (col + dCol)

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

pickOrDrop :: Level -> Cursor -> Maybe Pick -> Either String (Either Pick Edit)
pickOrDrop level (Cursor row col) Nothing
  | col == 0                     = Right (Left (PickClause row))
  | col <= length (rowGoals level row) = Right (Left (PickGoal row col))
  | otherwise                    = Left "Nothing to move here."
pickOrDrop level (Cursor row _) (Just (PickClause from))
  | isQueryRow level row = Left "Clauses can only move among clauses."
  | otherwise            = Right (Right (MoveClause from row))
pickOrDrop level (Cursor row col) (Just (PickGoal pickRow from))
  | row /= pickRow = Left "A goal can only move within its own clause or query."
  | otherwise      = Right (Right (MoveGoal (target level row) from to))
  where to = min col (length (rowGoals level row))

cutAt :: Level -> Cursor -> Either String Edit
cutAt level (Cursor row col) = Right (AddCut (target level row) (max 1 col))

uncutAt :: Level -> Cursor -> Either String Edit
uncutAt level (Cursor row col)
  | col >= 1, col <= length goals, goals !! (col - 1) == T.pack "!" =
      Right (RemoveCut (target level row) col)
  | otherwise = Left "Select a cut to remove it."
  where goals = rowGoals level row
