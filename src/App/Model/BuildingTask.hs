module App.Model.BuildingTask where

import App.Prelude

import App.Model.Resource (Resource)

data BuildingTask = BuildingTask
  { minedMineral :: Resource
  , finishTime :: Int
  }
  deriving (Show, Generic)
