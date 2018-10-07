module App.Model.BuildingTask where

import App.Prelude

import App.Model.Installation (Installation)

data BuildingTask = BuildingTask
  { installation :: Installation
  , quantity :: Int
  , finishTime :: Int
  }
  deriving (Show, Generic)
