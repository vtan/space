module App.Model.BuildingTask where

import App.Prelude

import App.Model.Installation (Installation)
import App.Model.Resource (Resource)

data BuildingTask = BuildingTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , resourcesSpent :: HashMap Resource Double
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)
