module Tui.Names (Name (..)) where

data Name
  = DungeonView
  | ProgramView
  | QueryView
  | ChronicleView
  | CacheHeader
  | CacheFooter
  | CacheProgram
  | CacheQuery
  | CacheChronicle
  deriving (Eq, Ord, Show)
