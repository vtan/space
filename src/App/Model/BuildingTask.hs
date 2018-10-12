module App.Model.BuildingTask where

import App.Prelude

import App.Dimension.Time (Time)
import App.Model.Installation (Installation)

data BuildingTask = BuildingTask
  { installation :: Installation
  , quantity :: Int
  , finishTime :: Time Int
  }
  deriving (Show, Generic)
