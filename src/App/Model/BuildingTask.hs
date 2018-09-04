module App.Model.BuildingTask where

import App.Prelude

import App.Model.BodyMinerals (Mineral)

data BuildingTask = BuildingTask
  { minedMineral :: Mineral
  , finishTime :: Int
  }
  deriving (Show, Generic)