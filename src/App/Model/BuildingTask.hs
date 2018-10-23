module App.Model.BuildingTask where

import App.Prelude

import App.Model.Installation (Installation)
import Data.String (fromString)

data BuildingTask = BuildingTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)

print :: BuildingTask -> Text
print BuildingTask{..} =
  fromString $
    printf "%s (%d)" (show installation) quantity
      ++ (if installWhenDone then " (install)" else "")
