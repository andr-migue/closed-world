{-# LANGUAGE OverloadedStrings #-}
module Protocol
  ( LevelSummary (..)
  , Level (..)
  , Run (..)
  , Step (..)
  , Room (..)
  , Door (..)
  , ClauseParts (..)
  , Tool (..)
  , Par (..)
  , Target (..)
  , Edit (..)
  , levelsRequest
  , runRequest
  ) where

import Data.Aeson
import Data.Text (Text)

data LevelSummary = LevelSummary
  { summaryId      :: Int
  , summaryName    :: Text
  , summaryConcept :: Text
  }

data Level = Level
  { levelName        :: Text
  , levelConcept     :: Text
  , levelDescription :: Text
  , levelHint        :: Text
  , levelProgram     :: [Text]
  , levelClauses     :: [ClauseParts]
  , levelQuery       :: Text
  , levelQueryGoals  :: [Text]
  , levelTorch       :: Int
  , levelTools       :: [Tool]
  , levelEdits       :: Int
  , levelPar         :: Maybe Par
  }

data Par = Par
  { parTorch :: Int
  , parEdits :: Int
  }

data ClauseParts = ClauseParts
  { clauseHead  :: Text
  , clauseGoals :: [Text]
  }

data Tool = Tool
  { toolName :: Text
  , toolUses :: Int
  }

data Target = Clause Int | Query

data Edit
  = MoveClause Int Int
  | MoveGoal Target Int Int
  | AddCut Target Int
  | RemoveCut Target Int

data Run = Run
  { runLevel     :: Level
  , runResult    :: Text
  , runMessage   :: Text
  , runTorchUsed :: Int
  , runStars     :: Int
  , runSteps     :: [Step]
  , runRooms     :: [Room]
  , runDoors     :: [Door]
  }

data Step = Step
  { stepEvent  :: Text
  , stepClause :: Maybe Int
  , stepText   :: Text
  , stepRoom   :: Maybe Int
  }

data Room = Room
  { roomId       :: Int
  , roomParent   :: Maybe Int
  , roomRow      :: Int
  , roomCol      :: Int
  , roomLabel    :: Text
  , roomKind     :: Text
  , roomKindStep :: Int
  , roomGhost    :: Bool
  , roomStep     :: Int
  }

data Door = Door
  { doorRoom   :: Int
  , doorStatus :: Text
  , doorStep   :: Int
  }

instance FromJSON LevelSummary where
  parseJSON = withObject "LevelSummary" $ \o ->
    LevelSummary <$> o .: "id" <*> o .: "name" <*> o .: "concept"

instance FromJSON Level where
  parseJSON = withObject "Level" $ \o ->
    Level <$> o .: "name" <*> o .: "concept" <*> o .: "description" <*> o .:? "hint" .!= ""
          <*> o .: "program" <*> o .: "clauses" <*> o .: "query" <*> o .: "query_goals"
          <*> o .: "torch" <*> o .: "tools" <*> o .: "edits" <*> o .:? "par"

instance FromJSON Par where
  parseJSON = withObject "Par" $ \o ->
    Par <$> o .: "torch" <*> o .: "edits"

instance FromJSON ClauseParts where
  parseJSON = withObject "ClauseParts" $ \o ->
    ClauseParts <$> o .: "head" <*> o .: "goals"

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o ->
    Tool <$> o .: "tool" <*> o .: "uses"

instance ToJSON Edit where
  toJSON (MoveClause from to) =
    object ["edit" .= ("move_clause" :: Text), "from" .= from, "to" .= to]
  toJSON (MoveGoal target from to) =
    object ["edit" .= ("move_goal" :: Text), "target" .= target, "from" .= from, "to" .= to]
  toJSON (AddCut target pos) =
    object ["edit" .= ("add_cut" :: Text), "target" .= target, "pos" .= pos]
  toJSON (RemoveCut target pos) =
    object ["edit" .= ("remove_cut" :: Text), "target" .= target, "pos" .= pos]

instance ToJSON Target where
  toJSON (Clause n) = toJSON n
  toJSON Query      = String "q"

instance FromJSON Run where
  parseJSON = withObject "Run" $ \o ->
    Run <$> o .: "level" <*> o .: "result" <*> o .: "message" <*> o .: "torch_used" <*> o .: "stars"
        <*> o .: "steps" <*> o .: "rooms" <*> o .: "doors"

instance FromJSON Step where
  parseJSON = withObject "Step" $ \o ->
    Step <$> o .: "event" <*> o .:? "clause" <*> o .: "text" <*> o .:? "room"

instance FromJSON Room where
  parseJSON = withObject "Room" $ \o ->
    Room <$> o .: "id" <*> o .:? "parent" <*> o .: "row" <*> o .: "col"
         <*> o .: "label" <*> o .: "kind" <*> o .: "kind_step"
         <*> o .: "ghost" <*> o .: "step"

instance FromJSON Door where
  parseJSON = withObject "Door" $ \o ->
    Door <$> o .: "room" <*> o .: "status" <*> o .: "step"

levelsRequest :: Value
levelsRequest = object ["op" .= ("levels" :: Text)]

runRequest :: Int -> [Edit] -> Value
runRequest n edits = object ["op" .= ("run" :: Text), "id" .= n, "edits" .= edits]
