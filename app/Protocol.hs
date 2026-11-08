{-# LANGUAGE OverloadedStrings #-}
module Protocol
  ( LevelSummary (..)
  , Level (..)
  , Run (..)
  , Step (..)
  , Room (..)
  , Door (..)
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
  , levelProgram     :: [Text]
  , levelQuery       :: Text
  , levelTorch       :: Int
  }

data Run = Run
  { runLevel     :: Level
  , runResult    :: Text
  , runMessage   :: Text
  , runTorchUsed :: Int
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
    Level <$> o .: "name" <*> o .: "concept" <*> o .: "description"
          <*> o .: "program" <*> o .: "query" <*> o .: "torch"

instance FromJSON Run where
  parseJSON = withObject "Run" $ \o ->
    Run <$> o .: "level" <*> o .: "result" <*> o .: "message" <*> o .: "torch_used"
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

runRequest :: Int -> Value
runRequest n = object ["op" .= ("run" :: Text), "id" .= n, "edits" .= ([] :: [Value])]
